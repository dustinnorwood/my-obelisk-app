{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api.Users
  ( UsersApi
  , module Namespace
  , module User
  , module Users
  ) where

import           Servant.API                             ((:<|>), (:>),
                                                          JSON, Post,
                                                          PostCreated, ReqBody)

import           Common.Api.Namespace         as Namespace (Namespace(Namespace))
import           Common.Api.User.Account      as User  (Account(Account))
import           Common.Api.Users.Credentials as Users (Credentials(Credentials))
import           Common.Api.Users.Registrant  as Users (Registrant(Registrant))

type UsersApi token =
  ( "login"
    :> ReqBody '[JSON] (Namespace "user" Credentials)
    :> Post '[JSON] (Namespace "user" Account)
  ) :<|> (
    ReqBody '[JSON] (Namespace "user" Registrant)
    :> PostCreated '[JSON] (Namespace "user" Account)
  )