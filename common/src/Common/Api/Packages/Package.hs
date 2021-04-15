{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
module Common.Api.Packages.Package where

import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Int                    (Int32)
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import GHC.Generics                (Generic)

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
