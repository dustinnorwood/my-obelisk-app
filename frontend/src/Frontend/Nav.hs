{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, TypeFamilies                                                               #-}
module Frontend.Nav where

import Reflex.Dom.Core

import Data.Bool              (bool)
import Data.Functor           (void)
import qualified Data.Map.Strict as M
import Data.Maybe             (fromMaybe)
import Data.Text              (Text)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, askRoute, setRoute)

import qualified Common.Api.User.Account as Account
import           Common.Route                    (homeRoute, FrontendRoute (..), Username (..), Windowed(..), GetPackagesParams(..))
import           Frontend.Client                 (urlGET)
import           Frontend.FrontendStateT
import           Frontend.Utils                  (routeLinkDynClass)

nav
  :: ( DomBuilder t m
     , Prerender js t m
     , PostBuild t m
     , MonadHold t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
nav = do
  rDyn <- askRoute
  loggedIn <- reviewFrontendState loggedInAccount
  el "header" . elClass "nav" "navbar navbar-expand-md navbar-light fixed-top bg-light" $ do
    routeLinkDynClass "navbar-brand" (constDyn $ homeRoute) $ text "Vaycon"
    elAttr "button" ("class"=:"navbar-toggler"
                    <> "type"=:"button"
                    <> "data-toggle"=:"collapse"
                    <> "data-target"=:"#navbarCollapse"
                    <> "aria-controls"=:"navbarCollapse"
                    <> "aria-expanded"=:"false"
                    <> "aria-label"=:"Toggle navigation"
                    ) $
        elClass "span" "navbar-toggler-icon" blank
    elAttr "div" ("class" =: "collapse navbar-collapse" <> "id"=:"navbarCollapse") $ do
      elClass "ul" "navbar-nav mr-auto" $ do
        navItem homeRoute rDyn $ text "Home"
        void $ widgetHold
          loggedOutMenu
          (maybe loggedOutMenu loggedInMenu <$> updated loggedIn)
        searchWidget

  where
    loggedOutMenu = do
      rDyn <- askRoute
      navItem (FrontendRoute_Login :/ ()) rDyn $ do
        text "Sign in"
      navItem (FrontendRoute_Register :/ ()) rDyn $ do
        text "Sign up"

    loggedInMenu a = do
      rDyn <- askRoute
      navItem (FrontendRoute_Settings :/ ()) rDyn $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      navItem
        (FrontendRoute_Profile :/ (Username "me",Nothing))
        rDyn $ do
          fbMap <- urlGET . constDyn $ "https://graph.facebook.com/v10.0/me?fields=short_name&access_token=" <> a
          shortName <- holdDyn "" $ fromMaybe "Vaycon User" . M.lookup ("short_name" :: Text) . fromMaybe M.empty <$> fbMap
          dynText shortName

    navItem r rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== r) <$> rDyn)
      (constDyn r)

searchWidget
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
searchWidget = do
  divClass "ui fluid action input" $ do
    -- st <- divClass "ui compact menu search__dropdown" $ do
    --   divClass "ui simple dropdown item" $ mdo
    --     elClass "i" "dropdown icon" blank
    --     (rk, txc) <- divClass "menu" $ do
    --       (r,_) <- elAttr' "div" ("class" =: "item") $ text "Beef Hunt"
    --       (t,_) <- elAttr' "div" ("class" =: "item") $ text "Water World"
    --     return curSearchType
    ti <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Search term..." <> "style" =: "border-radius: 0;")
    setRoute (updated $ (\t -> FrontendRoute_Home :/ (Windowed Nothing Nothing (GetPackagesParams (Just t) Nothing))) <$> value ti)
    return ()