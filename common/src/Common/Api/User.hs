{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Api.User
  ( UserApi
  , module Namespace
  , module Account
  , module UpdateUser
  ) where

import Servant.API  ((:<|>), (:>), Get, JSON, Put, ReqBody)

import Common.Api.Namespace    as Namespace (Namespace (Namespace))
import Common.Api.User.Account as Account (Account (Account), Token (Token))
import Common.Api.User.Update  as UpdateUser (UpdateUser (UpdateUser))

type UserApi =
  (
    Get '[JSON] (Namespace "user" Account)
  ) :<|> (
    ReqBody '[JSON] (Namespace "user" UpdateUser)
    :> Put '[JSON] (Namespace "user" Account)
  )
