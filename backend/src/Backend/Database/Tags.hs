{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Backend.Database.Tags
  ( create
  , query
  , TagT(..)
  , Tag
  , TagId
  ) where

import Control.Monad.Fail              (MonadFail)
import Control.Monad.IO.Class          (MonadIO)
import Control.Monad.Reader.Class      (MonadReader, ask)
import Control.Monad.Trans.Control     (MonadBaseControl)
import Data.Foldable                   (toList)
import Data.Set                        (Set)
import Data.Text                       (Text)
import Database.Beam.Postgres.Extended (all_, conflictingFields, insertExpressions, insertReturning,
                                        onConflict, onConflictUpdateInstead, runInsertReturning, runSelect,
                                        select, val_)
import Database.PostgreSQL.Simple      (Connection)

import           Backend.Database          (VayconDb (..), vayconDb, rowList)
import           Backend.Database.Tags.Tag (Tag, TagId, TagT (Tag))
import qualified Backend.Database.Tags.Tag as Tag

query
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => m [Tag]
query = do
  conn <- ask
  runSelect conn (select (all_ (vayconTags vayconDb))) rowList

create
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Set Text
  -> m [Tag]
create names = do
  conn <- ask
  runInsertReturning
    conn
    (insertReturning
       (vayconTags vayconDb)
       (insertExpressions (map (Tag . val_) (toList names)))
       (onConflict (conflictingFields Tag.name) (onConflictUpdateInstead Tag.name))
       (Just id))
    rowList
