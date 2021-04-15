{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, StandaloneDeriving, TypeOperators #-}
module Common.Api.Profiles
  ( ProfilesApi
  , Profile(Profile)
  ) where

import Data.Text    (Text)
import Servant.API  ((:>), Capture, Get, JSON)
import Servant.Auth (Auth, JWT)

import Common.Api.Namespace        (Namespace)
import Common.Api.Profiles.Profile

type ProfilesApi = Capture "username" Text :> Get '[JSON] (Namespace "profile" Profile)
