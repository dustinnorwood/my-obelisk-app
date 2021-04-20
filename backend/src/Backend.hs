{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import           Backend.Config
import           Backend.Database
import           Backend.Login
import           Backend.Monad
import           Common.Api                      (api)
import           Common.Route
import           Control.Exception.Safe          (throwString)
import           Control.Lens                    ((^.))
import           Control.Monad.IO.Class          (liftIO)
import qualified Crypto.JOSE                      as HOSE
import qualified Crypto.JOSE.Types                as HOSE
import           Data.Aeson
import           Data.Dependent.Sum              (DSum ((:=>)))
import           Data.Functor.Identity           (Identity (..))
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import           Obelisk.OAuth.Authorization     (OAuth (..), RedirectUriParams (..))
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.ExecutableConfig.Lookup
import           Obelisk.Route                   hiding (decode, encode)
import "servant-snap" Servant                    (serveSnap)
import           Snap

getYolo :: Text -> IO Text
getYolo l = do
  configs <- liftIO $ getConfigs
  let l' = "backend/" <> l
      route = fromMaybe (error . T.unpack $ "Please fill in config: config/backend/" <> l') $
                T.decodeUtf8 <$> M.lookup l' configs
  pure route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      pgConnStr <- getYolo "pgConnStr"
      jwtKey    <- getYolo "jwtKey"
      let jwk   =
               HOSE.fromKeyMaterial
             . HOSE.OctKeyMaterial
             . HOSE.OctKeyParameters
             . HOSE.Base64Octets
             . T.encodeUtf8
             $  jwtKey
      env <- mkEnv pgConnStr -- jwk
      liftIO $ putStrLn "About to test the db connection. If ob run dies, check out config/backend/pgConnStr"
      _ <- openVayconDb (T.encodeUtf8 pgConnStr)

      cfg <- readBackendConfig
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg
      serve $ \case
        BackendRoute_Missing :=> Identity () -> do
          writeLBS "404"
        BackendRoute_OAuth :=> Identity (OAuth_RedirectUri :=> Identity p) -> case p of
          Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
          Just (RedirectUriParams code mstate) -> do
            handleOAuthCallback cfg code
            redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) mstate
        BackendRoute_Api :/ apiR  -> case apiR of
          ApiRoute_Users :/ _ -> runVayconServerM env $ serveSnap api server
          ApiRoute_User :/ _ -> do
            resp <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) >>= \case
              Left e -> pure $ Left e
              Right u -> pure $ Right u
            writeLBS $ encode resp
            runVayconServerM env $ serveSnap api server
          ApiRoute_Packages :/ _ -> runVayconServerM env $ serveSnap api server
          ApiRoute_Profiles :/ _ -> runVayconServerM env $ serveSnap api server
          ApiRoute_Tags :/ _ -> runVayconServerM env $ serveSnap api server
        BackendRoute_GetSearchExamples :=> Identity () -> do
          resp <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) >>= \case
            Left e -> pure $ Left e
            Right u -> do
              -- TODO: This should be generic, and dates determined automatically.
              let examples :: [(Text, Text)] =
                    [ ("Do a basic search", "sunny day"),
                      ("Use quotes for exact match", "\"great day\""),
                      ("Browse messages on a particular day", "during:2018-8-23"),
                      ("All messages in #general channel", "in:general"),
                      ("Messages by Andrew in #random channel", "in:random from:andrew"),
                      ("Messages in #general after August 2018", "after:2018-08-01 in:general"),
                      ("Messages in #random during mid 2016", "after:2016-08-01 before:2016-09-01 in:random")
                    ]
              pure $ Right (u, examples)
          writeLBS $ encode resp
  }
