{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Login
  ( authorizeUser,
    handleOAuthCallback,
  )
where

import Backend.Config
import Common.Facebook.Types.Auth
import Common.Route
import Control.Exception.Safe (throwString)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes, listToMaybe)
import Data.Profunctor (rmap)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Obelisk.OAuth.AccessToken
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)
import Snap
import Snap.Snaplet.Session
import Web.ClientSession

authorizeUser ::
  MonadSnap m =>
  BackendConfig ->
  -- | The route to redirect after signing in to Facebook
  Text ->
  m (Either NotAuthorized Text)
authorizeUser cfg r = do
  tok <- getAuthToken (_backendConfig_sessKey cfg)
  pure $ rmap id f tok
  where
    f = \case
      Nothing ->
        Left $ NotAuthorized_RequireLogin ll
      Just (_, v) -> case decode v of
        Nothing -> Left $ NotAuthorized_RequireLogin ll
        Just t -> Right $ _fbTokenResponse_accessToken t
    -- NOTE: The only reason we build the grantHref in the backend instead
    -- of the frontend (where it would be most appropriate) is because of a
    -- bug in obelisk missing exe-config (we need routeEnv) in the frontend
    -- post hydration.
    ll = mkFBLoginLink cfg $ Just r

setFBTokenToCookie :: MonadSnap m => BackendConfig -> FBTokenResponse -> m ()
setFBTokenToCookie cfg = setAuthToken (_backendConfig_sessKey cfg) . encode

mkFBLoginLink :: BackendConfig -> Maybe Text -> Text
mkFBLoginLink cfg mstate = authorizationRequestHref authUrl routeEnv enc r
  where
    r =
      AuthorizationRequest
        { _authorizationRequest_responseType = AuthorizationResponseType_Code,
          _authorizationRequest_clientId = _backendConfig_oauthClientID cfg,
          _authorizationRequest_redirectUri = Just $ \x -> BackendRoute_OAuth :/ x,
          _authorizationRequest_scope = [],
          _authorizationRequest_state = mstate
        }
    authUrl = "https://www.facebook.com/v10.0/dialog/oauth"
    routeEnv = _backendConfig_routeEnv cfg
    enc = _backendConfig_enc cfg

handleOAuthCallback :: MonadSnap m => BackendConfig -> Text -> m ()
handleOAuthCallback cfg code = do
  let t =
        TokenRequest
          { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code,
            _tokenRequest_clientId = _backendConfig_oauthClientID cfg,
            _tokenRequest_clientSecret = _backendConfig_oauthClientSecret cfg,
            _tokenRequest_redirectUri = (\x -> BackendRoute_OAuth :/ x)
          }
      reqUrl = "https://graph.facebook.com/v10.0/oauth/access_token"
  req <-
    liftIO $
      getOauthToken
        reqUrl
        (_backendConfig_routeEnv cfg)
        (_backendConfig_enc cfg)
        t
  resp <- liftIO $ httpLbs req $ _backendConfig_tlsMgr cfg
  -- TODO: check response errors (both code and body json)!
  case decode (responseBody resp) of
    Nothing -> do
      -- TODO: When this throws {ok: false, error: ...} parse that properly
      liftIO $ T.putStrLn $ T.decodeUtf8 $ BL.toStrict $ responseBody resp
      liftIO $ throwString "Unable to decode JSON from Facebook oauth.access response"
    Just str -> do
      setFBTokenToCookie cfg str

getAuthToken :: MonadSnap m => Key -> m (Maybe (SecureCookie BL.ByteString))
getAuthToken k = do
  c <- getsRequest rqCookies
  fmap (join . listToMaybe . catMaybes) $ forM c $ \cc ->
    if cookieName cc == "vayconAuthToken"
      then
        let x :: Maybe (SecureCookie BL.ByteString) = decodeSecureCookie @BL.ByteString k (cookieValue cc)
         in pure $ Just x
      else pure Nothing

setAuthToken :: MonadSnap m => Key -> BL.ByteString -> m ()
setAuthToken k t = setSecureCookie "vayconAuthToken" Nothing k Nothing (t :: BL.ByteString)