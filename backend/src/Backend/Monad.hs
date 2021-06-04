{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, PackageImports, RankNTypes, TemplateHaskell, TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Backend.Monad
  ( module Backend.Monad
--  , Claim
  ) where

import Control.Lens hiding (Context, (??))

import           Control.Monad                    (void, when)
import           Control.Monad.FT
import           Control.Monad.Except             (ExceptT (ExceptT), runExceptT, withExceptT)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (asks, ReaderT, runReaderT)
-- import           Crypto.JOSE.JWK                  (JWK)
import           Data.List                        (sortOn)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe, maybeToList)
import           Data.Pool                        (Pool, createPool, withResource)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Database.Beam                    (primaryKey)
import           Database.PostgreSQL.Simple       (Connection, close)
import "servant-snap" Servant                     ((:<|>) ((:<|>)), Context ((:.), EmptyContext), Server)
import           Servant.Auth.Server              (CookieSettings,
                                                   JWTSettings, defaultCookieSettings, defaultJWTSettings)
import           Snap.Core                        (Snap)
import           Prelude                          hiding (lookup)


-- import           Backend.Claim                     (Claim (Claim), deriveToken)
import qualified Backend.Database                  as Database
import qualified Backend.Database.Packages         as DBPackages 
import qualified Backend.Database.Users            as DBUsers  
import           Backend.Database.Users.User       (dummyUser)
import qualified Backend.Database.Users.User       as DBUser
import qualified Backend.Database.Tags             as DBTags  
import           Backend.Errors
import           Common.Api                        as Api
import qualified Common.Api.Packages.Packages      as ApiPackages
import           Common.Api.Packages.Package       (PackageModel(..))
import qualified Common.Api.User.Account           as ApiAccount
import           Common.Route

data VayconServerEnv = VayconServerEnv
  { _dbPool      :: Pool Connection
  }
makeLenses ''VayconServerEnv

data VayconMemServerEnv = VayconMemServerEnv
  { _packages      :: Map Text PackageModel
  }
makeLenses ''VayconMemServerEnv

type VayconServerM   = ReaderT VayconServerEnv Snap
type VayconMemServerM = ReaderT VayconMemServerEnv Snap
type VayconServerDbM = VayconErrorsT (ReaderT Connection IO) --Concrete type for DB queries
type VayconServerContext = '[] --CookieSettings, JWTSettings]

newtype Prefixed a = Prefixed { unPrefixed :: a }

instance Selectable (Map Text PackageModel) GetPackages VayconMemServerM where
  select (Windowed l o (GetPackagesParams t f)) = do
    packageList <- M.toList <$> asks _packages
    let filteredForTags = case t of
                            Nothing -> packageList 
                            Just t' -> filter (\(_, PackageModel{..}) -> t' `Set.member` packageModelTags) packageList
    let filteredForFavs = case f of
                            Nothing -> filteredForTags 
                            Just f' -> filter (\(_, PackageModel{..}) -> f' `Set.member` packageModelFavorited) filteredForTags
    let sorted = reverse $ sortOn (\(_, PackageModel{..}) -> packageModelUpdatedAt) filteredForFavs
        windowed = take (fromInteger $ fromMaybe 20 l) $ drop (fromInteger $ fromMaybe 0 o) sorted
    pure $ if null windowed
             then Nothing
             else Just $ M.fromList windowed

instance Selectable (Prefixed (Map Text PackageModel)) SearchPackages VayconMemServerM where
  select (Windowed l o (SearchPackagesParams t)) = do
    packageList <- asks _packages
    let filtered = case T.toLower t of
          "" -> M.empty
          t' -> 
            let tags = M.filter (\(PackageModel{..}) ->
                         not
                       . null 
                       . filter (\pmt -> t' `T.isPrefixOf` (T.toLower pmt))
                       $ Set.toList packageModelTags
                       ) packageList
                favs = M.filter (\(PackageModel{..}) -> not
                                                     . null 
                                                     . filter (\pmt -> t' `T.isPrefixOf` (T.toLower pmt))
                                                     $ Set.toList packageModelFavorited
                                 ) packageList
                title = M.filter (\(PackageModel{..}) -> t' `T.isPrefixOf` (T.toLower packageModelTitle)
                                                      ) packageList
             in M.unions [tags, favs, title]
    let sorted = reverse $ sortOn (\(_, PackageModel{..}) -> packageModelUpdatedAt) $ M.toList filtered
        windowed = take (fromInteger $ fromMaybe 20 l) $ drop (fromInteger $ fromMaybe 0 o) sorted
    pure $ if null windowed
             then Nothing
             else Just . Prefixed $ M.fromList windowed

instance Selectable PackageModel Text VayconMemServerM where
  select slug = do
    packageList <- asks _packages
    pure $ M.lookup slug packageList

-- instance (Lookupable (Map Text PackageModel) GetPackages) VayconServerM where
--   lookup (Windowed l o (GetPackagesParams t f)) = runVayconErrorsT $ do
--       runDatabase $ do
--         -- currUserMay <- optionallyLoadAuthorizedUser authRes
--         let currUserMay = Just dummyUser
--         ApiPackages.fromList <$>
--           (DBPackages.all
--            (primaryKey <$> currUserMay)
--            (fromMaybe 20 l)
--            (fromMaybe 0 o)
--            (Set.fromList $ maybeToList t)
--            (Set.fromList $ maybeToList f))

runVayconServerM :: VayconServerEnv -> VayconServerM a -> Snap a
runVayconServerM e = flip runReaderT e

runVayconMemServerM :: VayconMemServerEnv -> VayconMemServerM a -> Snap a
runVayconMemServerM e = flip runReaderT e

mkEnv :: MonadIO m => Text -> m VayconServerEnv -- JWK -> m VayconServerEnv
mkEnv dbConnStr = do -- jwk = do
  p <- liftIO $ createPool (Database.openVayconDb (encodeUtf8 dbConnStr)) close 1 10 8
  pure $ VayconServerEnv p -- (defaultJWTSettings jwk)

server :: Server Api VayconServerContext VayconServerM
server = usersServer :<|> userServer :<|> packagesServer :<|> profileServer :<|> tagsServer

usersServer :: Server UsersApi VayconServerContext VayconServerM
usersServer = loginServer :<|> registerServer
  where
    loginServer (Namespace creds) = runVayconErrorsT $ do
      user   <- runDatabase $ do
        credMay <- liftQuery $ DBUsers.findByCredentials creds
        credMay ?? forbidden
      userToAccount user

    registerServer (Namespace registrant) = runVayconErrorsT $ do
      user <- runDatabase $ do
        validReg <- withExceptT failedValidation $ DBUsers.validateRegistrant registrant
        liftQuery $ DBUsers.create validReg
      userToAccount user

userServer :: Server UserApi VayconServerContext VayconServerM
userServer = currentUserServer :<|> updateUserServer
  where
    currentUserServer = runVayconErrorsT $ do
      -- user <- runDatabase $ loadAuthorizedUser authRes
      let user = dummyUser
      userToAccount user

    updateUserServer (Namespace update) = runVayconErrorsT $ do
      -- newUser <- runDatabase $ do
      --   currUser    <- loadAuthorizedUser authRes
      --   validUpdate <- withExceptT failedValidation $ DBUsers.validateUpdateUser currUser update
      --   liftQuery $ DBUsers.update (DBUser.unUserId (primaryKey currUser)) validUpdate
      let newUser = dummyUser
      userToAccount newUser

packagesServer :: Server PackagesApi VayconServerContext VayconServerM
packagesServer = listPackagesServer
            :<|> createPackageServer
            :<|> feedServer
            :<|> packageServer
  where
    listPackagesServer limit offset tags favorited = runVayconErrorsT $ do
      void . runDatabase $ do
        -- currUserMay <- optionallyLoadAuthorizedUser authRes
        let currUserMay = Just dummyUser
        ApiPackages.fromList <$>
          (DBPackages.all
           (primaryKey <$> currUserMay)
           (fromMaybe 20 limit)
           (fromMaybe 0 offset)
           (Set.fromList tags)
           (Set.fromList favorited))
      pure M.empty

    feedServer limit offset = runVayconErrorsT $ do
      runDatabase $ do
        -- currUser <- loadAuthorizedUser authRes
        let currUser = dummyUser
        ApiPackages.fromList <$>
          (DBPackages.feed
           (primaryKey currUser)
           (fromMaybe 20 limit)
           (fromMaybe 0 offset))

    createPackageServer (Namespace attrCreate) = runVayconErrorsT $ do
      runDatabase $ do
        -- currUser   <- loadAuthorizedUser authRes
        let currUser = dummyUser
        validAttrs <- withExceptT failedValidation $ DBPackages.validateAttributesForInsert attrCreate
        liftQuery $ Namespace <$> DBPackages.create (primaryKey currUser) validAttrs

    packageServer slug = getPackageServer
      where
        getPackageServer = runVayconErrorsT $ do
          package <- runDatabase loadPackage
          pure $ Namespace package

        loadPackage = do
          -- currUserMay <- optionallyLoadAuthorizedUser authRes
          let currUserMay = Just dummyUser
          packageMay  <- liftQuery $ DBPackages.find (primaryKey <$> currUserMay) slug
          packageMay ?? notFound ("Package(" <> slug <> ")")

profileServer :: Server ProfilesApi VayconServerContext VayconServerM
profileServer = profileGetServer
  where
    profileGetServer username = runVayconErrorsT $ do
      runDatabase $ do
        -- currUserMay <- optionallyLoadAuthorizedUser authRes
        let currUserMay = Just dummyUser
        profileMay  <- liftQuery $ DBUsers.findProfile (primaryKey <$> currUserMay) username
        Namespace <$> (profileMay ?? (notFound ("Profile(" <> username <> ")")))

tagsServer :: Server TagsApi VayconServerContext VayconServerM
tagsServer = tagsAllServer
  where
    tagsAllServer = runVayconErrorsT . runDatabase . liftQuery $ Namespace . fmap DBTags.name <$> DBTags.query

-- Helper Functions TODO Move to Internal Module -------------------------------------------------------------

userToAccount :: DBUser.User -> VayconErrorsT VayconServerM (Namespace "user" (ApiAccount.Account))
userToAccount user = do
  -- jwtS   <- view jwtSettings
  -- token  <- withExceptT (internalServerErrorShow "Couldn't make JWT") $ deriveToken jwtS user
  let token = "dummyToken"
  pure . Namespace $ Account
    { ApiAccount.email    = DBUser.email user
    , ApiAccount.token    = Api.Token token
    , ApiAccount.bio      = DBUser.bio user
    , ApiAccount.username = DBUser.username user
    , ApiAccount.image    = DBUser.image user
    }

-- We put ExceptT on top so it is easier to use (!?) and friends
runDatabase :: VayconServerDbM a -> VayconErrorsT VayconServerM a
runDatabase m = do
  p <- view dbPool
  -- TODO: There should probably be a transaction here ;)
  ExceptT . liftIO . withResource p $ runReaderT (runExceptT m)

liftQuery :: ExceptT Database.QueryError (ReaderT Connection IO) a -> VayconErrorsT (ReaderT Connection IO) a
liftQuery = withExceptT (internalServerErrorShow "QueryError")