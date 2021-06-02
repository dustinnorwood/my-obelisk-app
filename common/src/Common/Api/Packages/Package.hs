{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
module Common.Api.Packages.Package where

import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Int                    (Int32)
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import GHC.Generics                (Generic)

data PackageModel = PackageModel
  { packageModelTitle       :: Text
  , packageModelDescription :: Text
  , packageModelImage       :: Text
  , packageModelBody        :: Text
  , packageModelCreatedAt   :: UTCTime
  , packageModelUpdatedAt   :: UTCTime
  , packageModelTags        :: Set Text -- TODO: Tag
  , packageModelFavorited   :: Set Text -- TODO: UserId
  } deriving (Eq, Show)

deriving instance Generic PackageModel
deriving instance ToJSON PackageModel
deriving instance FromJSON PackageModel

data Package = Package
  { id             :: Int32
  , slug           :: Text
  , title          :: Text
  , description    :: Text
  , image          :: Text
  , body           :: Text
  , tagList        :: Set Text
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  , favorited      :: Bool
  , favoritesCount :: Int32
  } deriving Show

deriving instance Generic Package
deriving instance ToJSON Package
deriving instance FromJSON Package
