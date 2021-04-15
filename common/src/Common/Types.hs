module Common.Types where

import Common.Facebook.Types.Auth
import Data.Text (Text)

type ExamplesResponse =
  Either
    NotAuthorized
    (Text, [(Text, Text)])