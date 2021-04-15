{-# LANGUAGE OverloadedStrings #-}
module Backend.Validation
  ( requiredText
  , ValidationErrors
  ) where

import           Data.Functor.Compose (Compose (Compose))
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Validation      (Validation (Failure, Success))

import Common.Api.Validation (ValidationErrors)

requiredText
  :: (Applicative m)
  => Text
  -> Text
  -> Compose m (Validation ValidationErrors) Text
requiredText attr value =
  Compose $
  pure $
  if Text.null value
    then Failure (Map.singleton attr ["Required"])
    else Success value
