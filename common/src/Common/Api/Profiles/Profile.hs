{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Common.Api.Profiles.Profile
  ( Profile(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Int     (Int32)
import           Data.Text    (Text)
import           GHC.Generics (Generic)


data Profile = Profile
  { id        :: Int32
  , username  :: Text
  , bio       :: Text
  , image     :: Maybe Text
  , following :: Bool
  } deriving (Generic, Eq, Show)

deriving instance ToJSON Profile
deriving instance FromJSON Profile
