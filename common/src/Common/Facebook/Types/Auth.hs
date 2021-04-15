{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Types for working with Facebook's OAuth interface
module Common.Facebook.Types.Auth where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Text.Casing (fromHumps, toQuietSnake)

data FBTokenResponse = FBTokenResponse
  { _fbTokenResponse_accessToken :: Text,
    _fbTokenResponse_tokenType :: Text,
    _fbTokenResponse_expiresIn :: Integer
  }
  deriving (Eq, Show, Generic)

data FBUser = FBUser
  { _fbUser_name :: Text,
    _fbUser_id :: Text
  }
  deriving (Eq, Show, Generic)

data NotAuthorized
  = NotAuthorized_RequireLogin Text
  deriving (Eq, Show, Generic)

-- | Extract the slack login link out of NotAuthorized
notAuthorizedLoginLink :: NotAuthorized -> Text
notAuthorizedLoginLink = \case
  NotAuthorized_RequireLogin l -> l

-- | FB's OAuth JSON field label modifier
fieldLabelMod :: Options
fieldLabelMod =
  defaultOptions
    { fieldLabelModifier =
        toQuietSnake . fromHumps . drop 1 . dropWhile (/= '_') . drop 1
    }

instance FromJSON FBTokenResponse where
  parseJSON = genericParseJSON fieldLabelMod

instance FromJSON FBUser where
  parseJSON = genericParseJSON fieldLabelMod

instance FromJSON NotAuthorized where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON FBTokenResponse where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON FBUser where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON NotAuthorized where
  toJSON = genericToJSON fieldLabelMod