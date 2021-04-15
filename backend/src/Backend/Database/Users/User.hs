{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
module Backend.Database.Users.User
  ( UserT(..)
  , User
  , UserId
  , PrimaryKey(UserId, unUserId)
  , dummyUser
  ) where

import Prelude hiding (id)

import Data.Int      (Int32)
import Data.Text     (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data UserT f = User
  { id       :: Columnar f Int32
  , password :: Columnar f Text
  , email    :: Columnar f Text
  , username :: Columnar f Text
  , bio      :: Columnar f Text
  , image    :: Columnar f (Maybe Text)
  }

deriving instance Generic (UserT f)
deriving instance Beamable UserT

type User = UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance Ord User

instance Table UserT where
  data PrimaryKey UserT f = UserId
    { unUserId :: Columnar f Int32
    }
  primaryKey = UserId . id

deriving instance Generic (PrimaryKey UserT f)
deriving instance Beamable (PrimaryKey UserT)

type UserId = PrimaryKey UserT Identity

deriving instance Show UserId
deriving instance Eq UserId
deriving instance Ord UserId

dummyUser :: User
dummyUser = User 12345 "123456" "dummy@email.com" "Dummy" "I'm a dummy" Nothing
