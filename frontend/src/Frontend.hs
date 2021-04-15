{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.Route
import Common.Facebook.Types.Auth
import Common.Types
import Control.Monad
import Data.Functor.Identity (Identity)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Frontend.Util
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Element (scrollIntoView)
import GHCJS.DOM.Window (scrollTo)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom.Core

getSearchExamples ::
  (MonadHold t m, PostBuild t m, Prerender js t m) =>
  m (Event t (Maybe ExamplesResponse))
getSearchExamples = do
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ (renderBackendRoute enc $ (BackendRoute_GetSearchExamples :/ ())) <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      divClass "ui container" $ mdo
        divClass ("ui top attached inverted segment " <> themeColor)
          $ routeLink (FrontendRoute_Main :/ ())
          $ elClass "h1" "ui inverted header"
          $ text "Vaycon - Click your way to the weekend"
        highlightE :: Event t (Maybe (Element EventResult GhcjsDomSpace t)) <-
          fmap switchDyn $ prerender (pure never)
            $ delay 0.2
            $ updated highlight -- Delay until DOM is ready.
        void $ prerender blank $ widgetHold_ blank $ ffor highlightE $ \case
          Nothing ->
            currentWindowUnchecked >>= \w -> scrollTo w 0 0
          Just e ->
            scrollIntoView (_element_raw e) True
        highlight :: Dynamic t (Maybe (Element EventResult GhcjsDomSpace t)) <- divClass "ui attached segment" $ do
          fmap join $ subRoute $ \case
            FrontendRoute_Main -> do
              resp <- getSearchExamples
              widgetHold_ loader $ ffor resp $ \case
                Nothing -> text "Unable to load search examples"
                Just (Left na) -> notAuthorizedWidget na
                Just (Right (_, examples)) -> do
                  elHeader "h2" (text "Search examples") $
                    text "Begin browsing the archive using these search queries!"
                  divClass "ui two column centered grid" $ divClass "column"
                    $ divClass "ui list"
                    $ forM_ examples
                    $ \(title, query) -> do
                      divClass "item" $ divClass "ui piled segment" $ do
                        divClass "description" $ text title
                  el "p" blank
              pure $ constDyn Nothing
        divClass "ui bottom attached secondary segment" $ do
          elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
    }
  where
    loader :: DomBuilder t m => m ()
    loader = divClass "ui loading segment" blank
    notAuthorizedWidget :: DomBuilder t m => NotAuthorized -> m ()
    notAuthorizedWidget = \case
      NotAuthorized_RequireLogin grantHref -> divClass "ui segment" $ do
        el "p" $ text "You must login to Facebook to access this page."
        fbLoginButton grantHref
      where
        fbLoginButton r =
          elAttr "a" ("href" =: r) $
            elAttr "img" ("src" =: static @"fb.png"
                       <> "width" =: "400"
                         ) $ blank