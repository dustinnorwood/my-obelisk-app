{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Backend.Database.Packages
  ( all
  , create
  , destroy
  , addToWishlist
  , feed
  , find
  , unsafeFind
  , removeFromWishlist
  , update
  , validateAttributesForUpdate
  , validateAttributesForInsert
  ) where

import Prelude hiding (all)

import           Control.Lens                    (view, (^.), _1, _2, _3, _4)
import           Control.Monad                   (unless)
import           Control.Monad.FT                hiding (delete, insert, select, update)
import           Control.Monad.Error.Class       (MonadError, throwError)
import           Control.Monad.Fail              (MonadFail)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader.Class      (MonadReader, ask)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.Char                       as Char
import           Data.Foldable                   (for_, traverse_)
import           Data.Functor                    (void)
import           Data.Functor.Compose            (Compose (..))
import           Data.Functor.Identity           (Identity)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time                       (UTCTime, getCurrentTime)
import           Data.Validation                 (Validation (Failure, Success), validation)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vector
import           Database.Beam.Postgres.Extended (Nullable, PgInsertReturning, PgQExpr, Postgres, Postgres,
                                                  PgUpdateReturning, Q, aggregate_, all_, array_,
                                                  conflictingFields, count_, default_, delete, desc_, exists_,
                                                  group_, guard_, in_, insert, insertExpressions,
                                                  insertReturning, insertValues, isSubsetOf_, just_,
                                                  leftJoin_, limit_, offset_, onConflict, onConflictDefault,
                                                  onConflictDoNothing, orderBy_, pgArrayAgg, pgBoolOr,
                                                  primaryKey, references_, runDelete, runInsert,
                                                  runInsertReturning, runSelect, runUpdateReturning, select,
                                                  updateReturning, val_, (&&.), (<-.), (==.))
import           Database.PostgreSQL.Simple      (Connection)


import           Backend.Database                (VayconDb (..), QueryError (..), vayconDb,
                                                   maybeRow, rowList, singleRow)
import           Backend.Database.Packages.Package    (PackageT)
import qualified Backend.Database.Packages.Package    as Persisted
import           Backend.Database.Packages.PackageTag (PackageTagT (PackageTag))
import qualified Backend.Database.Packages.PackageTag as PackageTag
import           Backend.Database.Packages.Wishlist   (WishlistT (..))
import qualified Backend.Database.Packages.Wishlist   as Wishlist
import qualified Backend.Database.Tags                as Tag
import           Backend.Database.Tags.Tag            (PrimaryKey (unTagId))
import           Backend.Database.Users.User          (PrimaryKey (unUserId), UserId,
                                                               UserT (username))
import           Backend.Validation                   (ValidationErrors, requiredText)
import           Common.Api.Packages.Package          (Package (Package))
import qualified Common.Api.Packages.Package          as Package
import           Common.Api.Packages.Attributes       (PackageAttributes (..))

insertPackage
  :: UTCTime -> PackageAttributes Identity -> PgInsertReturning Persisted.Package
insertPackage currentTime PackageAttributes { title, description, image, body }
  = insertReturning
    (vayconPackages vayconDb)
    (insertExpressions
      [ Persisted.Package
          { Persisted.id          = default_
          , Persisted.slug        = val_ (generateSlug title)
          , Persisted.title       = val_ title
          , Persisted.description = val_ description
          , Persisted.image       = val_ image
          , Persisted.body        = val_ body
          , Persisted.createdAt   = val_ currentTime
          , Persisted.updatedAt   = val_ currentTime
          }
      ]
    )
    onConflictDefault
    (Just id)

create
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => UserId
  -> PackageAttributes Identity
  -> m Package
create authorId attributes = do
  conn <- ask
  currentTime <- liftIO getCurrentTime
  inserted <-
    runInsertReturning
      conn
      (insertPackage currentTime attributes)
      singleRow
  replaceTags (primaryKey inserted) (tagList attributes)
  unsafeFind (Just authorId) (Persisted.slug inserted)

find
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Maybe UserId
  -> Text
  -> m (Maybe Package)
find currentUserId slug = do
  conn <- ask
  fmap toPackage <$>
    runSelect conn (select (selectPackage currentUserId slug)) maybeRow

unsafeFind
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Maybe UserId
  -> Text
  -> m Package
unsafeFind currentUserId slug = do
  conn <- ask
  toPackage <$>
    runSelect conn (select (selectPackage currentUserId slug)) singleRow

updatePackage
  :: UTCTime -> Text -> PackageAttributes Maybe -> PgUpdateReturning Persisted.Package
updatePackage currentTime currentSlug PackageAttributes { title, description, image }
  = updateReturning
    (vayconPackages vayconDb)
    (\package -> mconcat $ catMaybes
      [ (Persisted.slug package <-.) . val_  . generateSlug <$> title
      , (Persisted.title package <-.) . val_ <$> title
      , (Persisted.description package <-.) . val_ <$> description
      , (Persisted.image package <-.) . val_ <$> image
      , Just (Persisted.updatedAt package <-. val_ currentTime)
      ]
    )
    ((val_ currentSlug ==.) . Persisted.slug)
    id

update
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => UserId
  -> Text
  -> PackageAttributes Maybe
  -> m Package
update authorId currentSlug attributes = do
  conn <- ask
  currentTime <- liftIO getCurrentTime
  updated <-
    runUpdateReturning
      conn
      (updatePackage currentTime currentSlug attributes)
      singleRow
  traverse_ (replaceTags (primaryKey updated)) (tagList attributes)
  unsafeFind (Just authorId) (Persisted.slug updated)

assignTags
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Persisted.PackageId
  -> Set Text
  -> m ()
assignTags packageId tags = do
  tagIds <- map primaryKey <$> Tag.create tags
  conn <- ask
  void $
    runInsert conn $
    insert
      (vayconPackageTags vayconDb)
      (insertValues (map (PackageTag packageId) tagIds))
      (onConflict (conflictingFields id) onConflictDoNothing)

deleteTags
  :: (MonadReader Connection m, MonadIO m)
  => Persisted.PackageId
  -> m ()
deleteTags packageId = do
  conn <- ask
  void $
    runDelete conn $
    delete
      (vayconPackageTags vayconDb)
      ((val_ packageId ==.) . PackageTag.package)

replaceTags
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Persisted.PackageId
  -> Set Text
  -> m ()
replaceTags packageId tags =
  deleteTags packageId *> assignTags packageId tags

destroy
  :: (MonadReader Connection m, MonadIO m)
  => Persisted.PackageId
  -> m ()
destroy packageId = do
  conn <- ask
  void $
    runDelete conn $
    delete (vayconPackages vayconDb) ((val_ packageId ==.) . primaryKey)

addToWishlist
  :: (MonadReader Connection m, MonadIO m)
  => Persisted.PackageId
  -> UserId
  -> m ()
addToWishlist package user = do
  conn <- ask
  void $
    runInsert
      conn $ insert
      (vayconWishlists vayconDb)
      (insertValues [Wishlist package user])
      onConflictDefault

removeFromWishlist
  :: (MonadReader Connection m, MonadIO m)
  => Persisted.PackageId
  -> UserId
  -> m ()
removeFromWishlist package user = do
  conn <- ask
  void $
    runDelete conn $
    delete (vayconWishlists vayconDb) $ \(Wishlist favPackage favUser) ->
      favUser ==. val_ user &&. favPackage ==. val_ package

type PackageRow s =
  ( PackageT (PgQExpr s)
  , PgQExpr s (Vector (Maybe Text))
  , PgQExpr s (Maybe Bool)
  , PgQExpr s Integer
  )

type PackageResult =
  ( Persisted.Package
  , Vector (Maybe Text)
  , Maybe Bool
  , Integer
  )

selectPackages :: Maybe UserId -> Q Postgres VayconDb s (PackageRow s)
selectPackages currentUserId =
  aggregate_
      (\(package, tag, wishlist') ->
        ( group_ package
        , pgArrayAgg $ unTagId (PackageTag.tag tag)
        , pgBoolOr
            (maybe
              (val_ False)
              ((Wishlist.user wishlist' ==.) . just_ . val_)
              currentUserId)
        , count_ $ unUserId (Wishlist.user wishlist')
        )
      )
    $ do
        package <- all_ (vayconPackages vayconDb)
        wishlist' <- selectWishlists package
        tag      <- selectTags package
        pure
          ( package
          , tag
          , wishlist'
          )

toPackage :: PackageResult -> Package
toPackage =
  Package
    <$> (Persisted.id . view _1)
    <*> (Persisted.slug . view _1)
    <*> (Persisted.title . view _1)
    <*> (Persisted.description . view _1)
    <*> (Persisted.image . view _1)
    <*> (Persisted.body . view _1)
    <*> (Set.fromList . catMaybes . Vector.toList . view _2)
    <*> (Persisted.createdAt . view _1)
    <*> (Persisted.updatedAt . view _1)
    <*> (fromMaybe False . view _3)
    <*> (fromIntegral . view _4)

selectFilteredPackages
  :: Maybe UserId
  -> Integer
  -> Integer
  -> Set Text
  -> Set Text
  -> Q Postgres VayconDb s (PackageRow s)
selectFilteredPackages currentUserId limit offset tags wishlisted
  = orderBy_ (desc_ . Persisted.createdAt . view _1)
    $ limit_ limit
    $ offset_ offset
    $ do
        package <- selectPackages currentUserId
        for_ (Set.toList tags) $ \tag ->
          guard_
            $ array_ [val_ (Just tag)] `isSubsetOf_` (package ^. _2)
        unless (null wishlisted) $ do
          wishlist' <- selectWishlists (package ^. _1)
          user     <- all_ (vayconUsers vayconDb)
          guard_
            $ (username user `in_` map val_ (Set.toList wishlisted)) &&.
              (just_ (primaryKey user) ==. Wishlist.user wishlist')
        pure package

all
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Maybe UserId
  -> Integer
  -> Integer
  -> Set Text
  -> Set Text
  -> m [Package]
all currentUserId limit offset tags wishlisted = do
  conn <- ask
  packages <-
    runSelect
      conn
      (select
         (selectFilteredPackages
            currentUserId
            limit
            offset
            tags
            wishlisted))
      rowList
  pure (toPackage <$> packages)

selectFeedPackages
  :: UserId
  -> Integer
  -> Integer
  -> Q Postgres VayconDb s (PackageRow s)
selectFeedPackages currentUserId limit offset = do
  package <- selectFilteredPackages (Just currentUserId) limit offset mempty mempty
  pure package

feed
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => UserId
  -> Integer
  -> Integer
  -> m [Package]
feed currentUserId limit offset = do
  conn <- ask
  packages <-
    runSelect
      conn
      (select (selectFeedPackages currentUserId limit offset))
      rowList
  pure (toPackage <$> packages)

selectPackage
  :: Maybe UserId
  -> Text
  -> Q Postgres VayconDb s (PackageRow s)
selectPackage currentUserId slug = do
  package <- selectPackages currentUserId
  guard_ $ Persisted.slug (package ^. _1) ==. val_ slug
  pure package

selectWishlists ::
     PackageT (PgQExpr s)
  -> Q Postgres VayconDb s (WishlistT (Nullable (PgQExpr s)))
selectWishlists package =
  leftJoin_
    (all_ (vayconWishlists vayconDb))
    ((`references_` package) . Wishlist.package)

selectTags
  :: PackageT (PgQExpr s)
  -> Q Postgres VayconDb s (PackageTagT (Nullable (PgQExpr s)))
selectTags package =
  leftJoin_
    (all_ (vayconPackageTags vayconDb))
    ((`references_` package) . PackageTag.package)

generateSlug :: Text -> Text
generateSlug = Text.intercalate "-" . Text.words . Text.toLower . Text.filter
  ((||) <$> Char.isAlphaNum <*> Char.isSpace)

slugExists
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m Bool
slugExists slug = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query :: Q Postgres VayconDb s (PgQExpr s Bool)
    query = pure $ exists_ $ do
      package <- all_ (vayconPackages vayconDb)
      guard_ (Persisted.slug package ==. val_ slug)
      pure package

titleGeneratingUniqueSlug
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Compose m (Validation ValidationErrors) Text
titleGeneratingUniqueSlug title =
  Compose $ do
    taken <- slugExists slug
    pure $
      if taken
        then Failure $
             Map.singleton "title" ["Would produce duplicate slug: " <> slug]
        else Success title
  where
    slug = generateSlug title

makeTitle
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Compose m (Validation ValidationErrors) Text
makeTitle title =
  requiredText "title" title *> titleGeneratingUniqueSlug title

validateAttributesForInsert
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     , MonadFail m
     )
  => PackageAttributes Identity
  -> m (PackageAttributes Identity)
validateAttributesForInsert PackageAttributes {..} =
  (validation throwError pure =<<) . getCompose $
  PackageAttributes
    <$> makeTitle title
    <*> requiredText "description" description
    <*> requiredText "image" image
    <*> requiredText "body" body
    <*> pure tagList

makeUpdateTitle
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Text
  -> Compose m (Validation ValidationErrors) Text
makeUpdateTitle current title
  | generateSlug title == current = requiredText "title" title
  | otherwise = makeTitle title

validateAttributesForUpdate
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     , MonadFail m
     )
  => Package
  -> PackageAttributes Maybe
  -> m (PackageAttributes Maybe)
validateAttributesForUpdate current PackageAttributes {..} =
  (validation throwError pure =<<) . getCompose $
  PackageAttributes
    <$> traverse (makeUpdateTitle (Package.slug current)) title
    <*> traverse (requiredText "description") description
    <*> traverse (requiredText "image") image
    <*> traverse (requiredText "body") body
    <*> pure tagList
