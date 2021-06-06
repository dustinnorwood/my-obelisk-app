{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Frontend.Search where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Control.Applicative    ((<|>))
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NEL
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Obelisk.Route          (R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute, setRoute)
import           Servant.Common.Req     (QParam (QNone, QParamSome))

import           Common.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Api.Packages.Packages (Packages (..))
import           Common.Route
import           Frontend.PackagePreview
import           Frontend.Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, buttonDynClass, routeLinkDynClass)
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)

data HomePageSelected = GlobalSelected | TagSelected Text deriving Show
makePrisms ''HomePageSelected

searchPage
  :: forall t m s js
  . ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     )
  => RoutedT t SearchPackages m ()
searchPage = do
  rDyn <- askRoute
  elClass "div" "search-page" $ mdo
    elClass "div" "banner" $
      elClass "div" "container" $ do
        elClass "h1" "logo-font" $ text "Undetermined Name Project"
        el "h1" $ dynText $ ffor rDyn $ \r -> "Search results" <> case r ^. item . search of
          "" -> ""
          s -> " for \"" <> s <> "\""

    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          elClass "div" "packages-toggle" $ do
            elClass "ul" "nav nav-pills outline-active" $ do
              searchResultsE <- backendGET $ (\term -> BackendRoute_Api :/ ApiRoute_Packages :/ PackagesRoute_Search :/ term) <$> rDyn
              searchResults <- holdDyn M.empty (fromMaybe M.empty <$> searchResultsE)
              let searchDropDownItem slug pkgModelDyn = do
                    (r,_) <- elAttr' "div" ("class" =: "item") $ packagePreview $ (\p ->(slug,p,Nothing)) <$> pkgModelDyn
                    pure (slug, slug <$ domEvent Click r)
              clickedDyn <- divClass "ui compact menu search__dropdown" $ do
                divClass "ui simple dropdown item" $ mdo
                  elClass "i" "dropdown icon" blank
                  divClass "menu" $ do
                    listWithKey searchResults searchDropDownItem
              let elemsDyn = M.elems <$> clickedDyn
                  clickedItem = switchDyn $ leftmost . map snd <$> elemsDyn
                  clearEv = "" <$ clickedItem
              setRoute ((\t -> FrontendRoute_Package :/ (DocumentSlug t)) <$> clickedItem)
              return ()
  pure ()
  where
    navItem q rDyn =
      let gp = rdef & item . term .~ q
       in elClass "li" "nav-item" . routeLinkDynClass
      ((\w -> ("nav-link " <>) . bool "" " active" $ (w ^. item . term) == q) <$> rDyn)
      (constDyn $ FrontendRoute_Home :/ gp)