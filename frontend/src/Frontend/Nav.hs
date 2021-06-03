{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, TypeFamilies, TupleSections #-}
module Frontend.Nav where

import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import Data.Bool              (bool)
import Data.Functor           (void)
import qualified Data.Map.Strict as M
import Data.Maybe             (fromMaybe)
import Data.Text              (Text)
import Data.Traversable       (for)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, askRoute, setRoute)

import qualified Common.Api.User.Account as Account
import           Common.Api.Packages.Package (PackageModel(..))
import           Common.Route
import           Frontend.Client                 (urlGET, backendGET)
import           Frontend.FrontendStateT
import           Frontend.PackagePreview
import           Frontend.Utils                  (routeLinkDynClass)

nav
  :: ( DomBuilder t m
     , Prerender js t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
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
     , RouteToUrl (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , Prerender js t m
     )
  => m ()
searchWidget = do
  divClass "ui fluid action input" $ mdo
    ti <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Search term..." <> "style" =: "border-radius: 0;")
    let search = (Windowed Nothing Nothing . SearchPackagesParams) <$> value ti
    searchResultsE <- backendGET $ (\term -> BackendRoute_Api :/ ApiRoute_Packages :/ PackagesRoute_Search :/ term) <$> search
    searchResults <- holdDyn M.empty (fromMaybe M.empty <$> searchResultsE)
    clickedDyn <- divClass "ui compact menu search__dropdown" $ do
      divClass "ui simple dropdown item" $ mdo
        elClass "i" "dropdown icon" blank
        divClass "menu" $ do
          listWithKey searchResults searchDropDownItem
    let clickedItem = switchDyn $ leftmost . M.elems <$> clickedDyn
    setRoute ((\t -> FrontendRoute_Package :/ (DocumentSlug t)) <$> clickedItem)
    return ()
  where searchDropDownItem slug pkgModelDyn = do
          (r,_) <- elAttr' "div" ("class" =: "item") $ packagePreviewSmall $ (slug,) <$> pkgModelDyn
          pure (slug <$ domEvent Click r)