{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Api
  ( module Common.Api
--  , module Articles
--  , module Namespace
--  , module Packages
--  , module Profiles
--  , module Tags
--  , module User
  , module Users
  ) where

import Data.Proxy  (Proxy (..))
import Servant.API ((:>)) -- (:<|>), (:>))

-- import Common.Conduit.Api.Articles  as Articles
-- import Common.Conduit.Api.Namespace as Namespace
-- import Common.Conduit.Api.Packages  as Packages
-- import Common.Conduit.Api.Profiles  as Profiles
-- import Common.Conduit.Api.User      as User
import Common.Api.Users     as Users
-- import Common.Conduit.Api.Tags      as Tags

type Api token = "api" :> TopLevelApi token

type TopLevelApi token
     =    ("users"     :> UsersApi token)
     -- :<|> ("user"      :> UserApi token)
     -- :<|> ("articles"  :> ArticlesApi token)
     -- :<|> ("packages"  :> PackagesApi token)
     -- :<|> ("profiles"  :> ProfilesApi token)
     -- :<|> ("tags"      :> TagsApi token)

api :: Proxy (Api token)
api = Proxy