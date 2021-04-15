{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Api.Packages.Packages where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))
import           Data.Int                               (Int32)
import           GHC.Generics                           (Generic)

import           Common.Api.Packages.Package (Package)

data Packages = Packages
  { packages      :: [Package]
  , packagesCount :: Int32
  } deriving Show

fromList :: [Package] -> Packages
fromList = Packages <$> id <*> fromIntegral . length

deriving instance Generic Packages
deriving instance ToJSON Packages
deriving instance FromJSON Packages
