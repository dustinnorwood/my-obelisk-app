{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell                                       #-}
module Frontend.HomePage where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NEL
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import           Obelisk.Route          (R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import           Servant.Common.Req     (QParam (QNone))

import           Common.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Api.Packages.Packages (Packages (..))
import           Common.Route                         (FrontendRoute (..))
import           Frontend.PackagePreview              (packagesPreview)
import qualified Frontend.Client                      as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, buttonDynClass)

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
  => m ()
homePage = elClass "div" "home-page" $ mdo
  tokDyn <- reviewFrontendState loggedInToken
  pbE <- getPostBuild
  let newSelection = leftmost [pbE, void $ updated tokDyn]

  (loadTagsE,_,_) <- Client.allTags newSelection

  tagsDyn <- holdDyn (Namespace []) loadTagsE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "Undetermined Name Project"
      el "p" $ text "Your weekend is just a click away"

  void . elClass "div" "container page" . elClass "div" "row" $ do
    elClass "div" "col-md-12" $ do
      el "h1" $ text "Popular"
      (loadPkgsE,_,pkgsLoadingDyn) <- do
        Client.listPackages
            (constDyn QNone)
            (constDyn QNone)
            (constDyn ["popular"])
            (constDyn [])
            newSelection
      pkgsDyn <- holdDyn (Packages [] 0) loadPkgsE
      packagesPreview pkgsLoadingDyn pkgsDyn

  void . elClass "div" "container page" . elClass "div" "row" $ do
    elClass "div" "col-md-12" $ do
      el "h1" $ text "Categories"
      (loadPkgsE,_,pkgsLoadingDyn) <- do
        Client.listPackages
            (constDyn QNone)
            (constDyn QNone)
            (constDyn ["categories"])
            (constDyn [])
            newSelection
      pkgsDyn <- holdDyn (Packages [] 0) loadPkgsE
      packagesPreview pkgsLoadingDyn pkgsDyn

  void . elClass "div" "container page" . elClass "div" "row" $ do
    elClass "div" "col-md-12" $ do
      el "h1" $ text "Top Rated"
      (loadPkgsE,_,pkgsLoadingDyn) <- do
        Client.listPackages
            (constDyn QNone)
            (constDyn QNone)
            (constDyn ["top-rated"])
            (constDyn [])
            newSelection
      pkgsDyn <- holdDyn (Packages [] 0) loadPkgsE
      packagesPreview pkgsLoadingDyn pkgsDyn


  void . elClass "div" "container page" . elClass "div" "row" $ do
    elClass "div" "col-md-12" $ do
      el "h1" $ text "We think you'll like"
      (loadPkgsE,_,pkgsLoadingDyn) <- do
        Client.listPackages
            (constDyn QNone)
            (constDyn QNone)
            (constDyn ["we-think-you-will-like"])
            (constDyn [])
            newSelection
      pkgsDyn <- holdDyn (Packages [] 0) loadPkgsE
      packagesPreview pkgsLoadingDyn pkgsDyn
  pure ()