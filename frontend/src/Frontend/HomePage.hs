{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell                                       #-}
module Frontend.HomePage where

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
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import           Servant.Common.Req     (QParam (QNone, QParamSome))

import           Common.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Api.Packages.Packages (Packages (..))
import           Common.Route
import           Frontend.PackagePreview              (packagesPreview)
import qualified Frontend.Client                      as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, buttonDynClass, routeLinkDynClass)
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)

data HomePageSelected = GlobalSelected | TagSelected Text deriving Show
makePrisms ''HomePageSelected

homePage
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
  => RoutedT t GetPackages m ()
homePage = do
  elClass "div" "home-page" $ mdo
    pbE <- getPostBuild
    tokDyn <- reviewFrontendState loggedInToken
    let newSelection = leftmost [pbE, void $ updated tokDyn]

    (loadTagsE,_,_) <- Client.allTags newSelection

    tagsDyn <- holdDyn (Namespace []) loadTagsE

    elClass "div" "banner" $
      elClass "div" "container" $ do
        elClass "h1" "logo-font" $ text "Undetermined Name Project"
        el "p" $ text "Your weekend is just a click away"

    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          elClass "div" "packages-toggle" $ do
            elClass "ul" "nav nav-pills outline-active" $ do
              rDyn <- askRoute
              let reload = leftmost [newSelection, () <$ updated rDyn]
              navItem Nothing rDyn $ text "Popular"
              navItem (Just "top-rated") rDyn $ text "Top Rated"
              navItem (Just "we-think-you-will-like") rDyn $ text "We Think You Will Like"
              navItem (Just "today") rDyn $ text "Today"
              navItem (Just "this-weekend") rDyn $ text "This Weekend"
              navItem (Just "price-low") rDyn $ text "$"
              navItem (Just "price-med") rDyn $ text "$$"
              navItem (Just "price-high") rDyn $ text "$$$"
              navItem (Just "price-lambo") rDyn $ text "$$$$"
              let ttt = (item . term %~ (Just . fromMaybe "popular")) <$> rDyn
              mPkgsE <- Client.backendGET $ (\r -> BackendRoute_Api :/ ApiRoute_Packages :/ PackagesRoute_Get :/ r) <$> rDyn
              pkgsDyn <- holdDyn (M.empty, False) $ fromMaybe (M.empty, False) <$> mPkgsE
              packagesPreview pkgsDyn
  pure ()
  where
    navItem q rDyn =
      let gp = Windowed Nothing Nothing (GetPackagesParams q Nothing)
       in elClass "li" "nav-item" . routeLinkDynClass
      ((\(Windowed _ _ (GetPackagesParams mq _)) -> ("nav-link " <>) . bool "" " active" $ mq == q) <$> rDyn)
      (constDyn $ FrontendRoute_Home :/ gp)