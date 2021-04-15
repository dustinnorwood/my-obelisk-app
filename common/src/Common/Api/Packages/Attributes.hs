{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Common.Api.Packages.Attributes where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Functor.Identity           (Identity)
import           Data.Set                        (Set)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Common.Api.Attribute (Attribute)

data PackageAttributes f = PackageAttributes
  { title       :: Attribute f Text
  , description :: Attribute f Text
  , image       :: Attribute f Text
  , body        :: Attribute f Text
  , tagList     :: Attribute f (Set Text)
  }

type CreatePackage = PackageAttributes Identity

deriving instance Generic CreatePackage
deriving instance ToJSON CreatePackage
deriving instance FromJSON CreatePackage

type UpdatePackage = PackageAttributes Maybe

deriving instance Generic UpdatePackage
deriving instance ToJSON UpdatePackage
deriving instance FromJSON UpdatePackage
