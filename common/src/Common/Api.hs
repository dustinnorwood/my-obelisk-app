{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Api
  ( module Common.Api
--  , module Articles
  , module Namespace
  , module Packages
  , module Profiles
  , module Tags
  , module User
  , module Users
  ) where

import Data.Proxy  (Proxy (..))
import Servant.API ((:>), (:<|>), (:>))

-- import Common.Api.Articles  as Articles
import Common.Api.Namespace as Namespace
import Common.Api.Packages  as Packages
import Common.Api.Profiles  as Profiles
import Common.Api.User      as User
import Common.Api.Users     as Users
import Common.Api.Tags      as Tags

type Api = "api" :> TopLevelApi

type TopLevelApi
     =    ("users"     :> UsersApi)
     :<|> ("user"      :> UserApi)
     :<|> ("packages"  :> PackagesApi)
     :<|> ("profiles"  :> ProfilesApi)
     :<|> ("tags"      :> TagsApi)

api :: Proxy Api
api = Proxy