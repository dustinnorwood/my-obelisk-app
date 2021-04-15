{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances                    #-}
module Backend.Database.Packages.Wishlist
  ( WishlistT(..)
  , Wishlist
  , PrimaryKey(WishlistId)
  ) where

import Database.Beam (Beamable, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

import Backend.Database.Packages.Package (PackageT)
import Backend.Database.Users.User       (UserT)

data WishlistT f = Wishlist
  { package :: PrimaryKey PackageT f
  , user    :: PrimaryKey UserT f
  }

deriving instance Generic (WishlistT f)
deriving instance Beamable WishlistT

type Wishlist = WishlistT Identity

deriving instance Show Wishlist

instance Table WishlistT where
  data PrimaryKey WishlistT f
    = WishlistId
        (PrimaryKey PackageT f)
        (PrimaryKey UserT f)
  primaryKey = WishlistId <$> package <*> user

deriving instance Generic (PrimaryKey WishlistT f)
deriving instance Beamable (PrimaryKey WishlistT)

type WishlistId = PrimaryKey WishlistT Identity

deriving instance Show WishlistId
deriving instance Eq WishlistId
