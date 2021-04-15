{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Config where

import Common.Route
import Control.Monad.IO.Class
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Natural (Natural)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Obelisk.ExecutableConfig.Lookup
import Obelisk.Route hiding (decode, encode)
import Web.ClientSession

data BackendConfig = BackendConfig
  { _backendConfig_enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName,
    _backendConfig_sessKey :: !Key,
    _backendConfig_tlsMgr :: !Manager,
    _backendConfig_pageSize :: !Natural,
    _backendConfig_routeEnv :: !Text,
    _backendConfig_oauthClientID :: !Text, -- TODO: invariant for oauth format?
    _backendConfig_oauthClientSecret :: !Text
  }

readBackendConfig :: MonadIO m => m BackendConfig
readBackendConfig = do
  configs <- liftIO $ getConfigs
  let route = fromMaybe "" $ decodeUtf8 <$> Map.lookup "common/route" configs
  let oauthClientID = fromMaybe "" $ T.strip . decodeUtf8 <$> Map.lookup "backend/oauthClientID" configs
  let oauthClientSecret = fromMaybe "" $ T.strip . decodeUtf8 <$> Map.lookup "backend/oauthClientSecret" configs
  k <- snd <$> liftIO randomKey
  tlsMgr <- liftIO newTlsManager
  pure $ BackendConfig enc k tlsMgr defaultPageSize route oauthClientID oauthClientSecret
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder
    -- WARNING: Changing the default page size will invalidate existing message
    -- permalinks (which contain page index embedded in them).
    --
    -- The only solution here is to get rid of page-index pagination and use
    -- ?afterTs or some such thing, while making sure frontend page navigation
    -- is still possible.
    defaultPageSize = 30
