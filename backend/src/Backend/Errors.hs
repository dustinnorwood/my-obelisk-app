{-# LANGUAGE FlexibleContexts, OverloadedStrings, PackageImports, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module Backend.Errors
  ( ErrorBody(..)
  , VayconErrorsT
  , (??)
  , failedValidation
  , forbidden
  , internalServerError
  , internalServerErrorShow
  , notAuthorized
  , notFound
  , runVayconErrorsT
  ) where

import Control.Exception
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, MonadError)
import Control.Monad.IO.Class
import Data.Aeson           (ToJSON, encode)
import Data.Text            (Text, pack)
import "servant-snap" Servant (ServantErr (..), err401, err403, err404, err500, errBody)
import Snap.Core            (MonadSnap)

import Common.Api.Errors

newtype VayconException = VayconException ServantErr
  deriving (Show, Exception)

throwVayconException :: MonadIO m => ServantErr -> m a
throwVayconException = liftIO . throwIO . VayconException

type VayconErrorsT m = ExceptT ServantErr m

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)

runVayconErrorsT :: MonadSnap m => VayconErrorsT m a -> m a
runVayconErrorsT = (either throwVayconException pure  =<<) . runExceptT

notAuthorized :: ServantErr
notAuthorized =
  err401 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Not Authorized"
        , errors = Nothing
        }

forbidden :: ServantErr
forbidden =
  err403 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Forbidden"
        , errors = Nothing
        }

notFound :: Text -> ServantErr
notFound resourceName =
  err404 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = resourceName <> " not found"
        , errors = Nothing
        }

failedValidation :: ToJSON failures => failures -> ServantErr
failedValidation failures =
  ServantErr
    { errHTTPCode = 422
    , errReasonPhrase = "Unprocessable Entity"
    , errBody = encode (body failures)
    , errHeaders = []
    }
    where
      body :: failures -> ErrorBody failures
      body fs = ErrorBody
        { message = "Failed validation"
        , errors = Just fs
        }

internalServerError :: Text -> ServantErr
internalServerError msg =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg]
        }

internalServerErrorShow :: Show e => Text -> e -> ServantErr
internalServerErrorShow msg e =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg, pack . show $ e]
        }
