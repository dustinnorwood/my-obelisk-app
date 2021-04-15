{-# LANGUAGE DeriveGeneric, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Backend.Database
  ( VayconDb(..)
  , QueryError(..)
  , VayconDb
  , vayconDb
  , maybeRow
  , openVayconDb
  , rowList
  , singleRow
  ) where

import           Control.Exception          (Exception)
import           Control.Monad.Error.Class  (MonadError, throwError)
import           Data.ByteString            (ByteString)
import           Data.Conduit               (ConduitT, (.|))
import qualified Data.Conduit               as Conduit
import qualified Data.Conduit.List          as Conduit
import           Data.Int                   (Int32)
import           Database.Beam              (Database, DatabaseSettings, MonadIO, TableEntity, dbModification,
                                             defaultDbSettings, fieldNamed, liftIO, modifyTable,
                                             tableModification, withDbModification)
import           Database.Beam.Postgres     (Postgres)
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           GHC.Generics               (Generic)


 
-- import           Backend.Database.Articles.Article    (ArticleT)
-- import qualified Backend.Database.Articles.Article    as Article
-- import           Backend.Database.Articles.ArticleTag (ArticleTagT)
-- import           Backend.Database.Articles.Favorite   (FavoriteT)
import           Backend.Database.Packages.Package    (PackageT)
import qualified Backend.Database.Packages.Package    as Package
import           Backend.Database.Packages.PackageTag (PackageTagT)
import           Backend.Database.Packages.Wishlist   (WishlistT)
-- import           Backend.Database.Comments.Comment    (CommentT)
-- import qualified Backend.Database.Comments.Comment    as Comment
import           Backend.Database.Tags.Tag            (TagT)
import           Backend.Database.Users.Follow        (FollowT)
import           Backend.Database.Users.User          (UserT)


data VayconDb f = VayconDb
  -- { vayconArticleTags :: f (TableEntity ArticleTagT)
  -- , vayconArticles    :: f (TableEntity ArticleT)
  -- , vayconComments    :: f (TableEntity CommentT)
  -- , vayconFavorites   :: f (TableEntity FavoriteT)
  { vayconPackageTags :: f (TableEntity PackageTagT)
  , vayconPackages    :: f (TableEntity PackageT)
  , vayconWishlists   :: f (TableEntity WishlistT)
  , vayconFollows     :: f (TableEntity FollowT)
  , vayconTags        :: f (TableEntity TagT)
  , vayconUsers       :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres VayconDb

newtype QueryError = UnexpectedAmountOfRows Int32
  deriving Show

instance Exception QueryError

vayconDb :: DatabaseSettings Postgres VayconDb
vayconDb =
  defaultDbSettings `withDbModification`
  dbModification
    --{ vayconArticles =
    --    modifyTable id $
    --    tableModification
    --      { Article.createdAt = fieldNamed "created_at"
    --      , Article.updatedAt = fieldNamed "updated_at"
    --      }
    { vayconPackages =
        modifyTable id $
        tableModification
          { Package.createdAt = fieldNamed "created_at"
          , Package.updatedAt = fieldNamed "updated_at"
          }
    --, vayconComments =
    --    modifyTable id $
    --    tableModification
    --      { Comment.createdAt = fieldNamed "created_at"
    --      , Comment.updatedAt = fieldNamed "updated_at"
    --      }
    }

openVayconDb :: MonadIO m => ByteString -> m Connection
openVayconDb = liftIO . connectPostgreSQL

maybeRow :: Monad m => ConduitT () a m () -> m (Maybe a)
maybeRow c = Conduit.runConduit (c .| Conduit.await)

singleRow :: (MonadError QueryError m) => ConduitT () a m () -> m a
singleRow c = maybe (throwError (UnexpectedAmountOfRows 0)) pure =<< maybeRow c

rowList :: Monad m => ConduitT () a m () -> m [a]
rowList c = Conduit.runConduit (c .| Conduit.consume)
