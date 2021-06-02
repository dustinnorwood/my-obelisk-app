{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Api.Packages
  ( PackagesApi
  , PackageApi
  -- , Windowed(..)
  -- , GetPackagesParams(..)
  -- , GetPackages
  -- , GetPackagesFeed
  , module Package
  , module Packages
  , module Attributes
  , module Namespace
  ) where

import Servant.API

import Data.Map.Strict    (Map)
import Data.Text    (Text)

import Common.Api.Packages.Package       as Package (PackageModel, Package (Package))
import Common.Api.Packages.Packages      as Packages (Packages (Packages))
import Common.Api.Packages.Attributes    as Attributes (PackageAttributes (PackageAttributes), CreatePackage,
                                                  UpdatePackage)
import Common.Api.Namespace              as Namespace (Namespace (Namespace))

data Windowed a = Windowed
  { _limit       :: Maybe Integer
  , _offset      :: Maybe Integer
  , _item        :: a
  }

data GetPackagesParams = GetPackagesParams
  { _tag         :: Maybe Text
  , _favorited   :: Maybe Text
  }

type GetPackages     = Windowed GetPackagesParams
type GetPackagesFeed = Windowed ()

type PackagesApi =
  (
     QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> QueryParams "tag" Text
  :> QueryParams "favorited" Text
  :> Get '[JSON] (Map Text PackageModel)
  ) :<|> (
     ReqBody '[JSON] (Namespace "package" CreatePackage)
  :> PostCreated '[JSON] (Namespace "package" Package)
  ) :<|> (
    "feed"
  :> QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> Get '[JSON] Packages
  )
  :<|> PackageApi


type PackageApi = (
  Capture "slug" Text
  :> (
    Get '[JSON] (Namespace "package" Package)
    )
  )
