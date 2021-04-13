{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.OAuth.Authorization (AuthorizationRequest (..), AuthorizationResponseType (..), authorizationRequestHref)
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      -- cfg <- getConfigs
      let route = T.strip $ "http://localhost:8000" -- T.decodeUtf8 $ cfg ! "common/route"

      el "h1" $ text "Welcome to Obelisk OAuth!"
      let r = AuthorizationRequest
            { _authorizationRequest_responseType = AuthorizationResponseType_Code
            , _authorizationRequest_clientId = "501263954211528"
            , _authorizationRequest_redirectUri = Just $ \x -> BackendRoute_OAuth :/ x
            , _authorizationRequest_scope = []
            , _authorizationRequest_state = Just "none"
            }
          grantHref = authorizationRequestHref "https://www.facebook.com/v10.0/dialog/oauth" route checkedEncoder r
      elAttr "a" ("href" =: grantHref) $ elAttr "img" ("src" =: (static @"fb.png")) $ blank
  }
