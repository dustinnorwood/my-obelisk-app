{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
module Frontend.PackagePreview where


import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           (void)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)

import Common.Route   (DocumentSlug (..), FrontendRoute (..), Username (..))
import Frontend.Utils (imgUrl, routeLinkDyn, routeLinkDynClass)

import           Common.Api.Packages.Package  (Package, PackageModel(..))
import qualified Common.Api.Packages.Package  as Package
import           Common.Api.Packages.Packages (Packages)
import qualified Common.Api.Packages.Packages as Packages
import qualified Common.Api.Profiles.Profile  as Profile

packagesPreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Bool
  -> Dynamic t (Map Text PackageModel)
  -> m ()
packagesPreview artsLoading artMapDyn = void . dyn . ffor artsLoading $ bool loaded loading
  where
    loaded = void . elClass "div" "row" $ dyn $ ffor artMapDyn $ \m ->
        if Map.null m
        then blankPreview "There is nothing here ... yet!"
        else void $ list (Map.mapWithKey (\k v -> (k, v)) <$> artMapDyn) $ packagePreview
    loading = blankPreview "Loading..."
    blankPreview = elClass "div" "package-preview" . el "em" . text

packagePreviewSmall
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => Dynamic t (Text, PackageModel) -> m ()
packagePreviewSmall packageDyn = elClass "div" "package-preview" $ do
  elClass "div" "package-meta" $ do
    packageImage "thumbnail" $ packageModelImage . snd <$> packageDyn
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Package :/ (DocumentSlug $ fst a)) <$> packageDyn)
      $ do
        el "h3" $ dynText $ packageModelTitle . snd <$> packageDyn

packagePreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => Dynamic t (Text, PackageModel) -> m ()
packagePreview packageDyn = elClass "div" "package-preview col-md-3" $ do
  elClass "div" "package-meta" $ do
    packageImage "" $ packageModelImage . snd <$> packageDyn
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      display $ S.size . packageModelFavorited . snd <$> packageDyn
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Package :/ (DocumentSlug $ fst a)) <$> packageDyn)
      $ do
        el "h3" $ dynText $ packageModelTitle . snd <$> packageDyn
    el "p" $ dynText $ (<>"...") . T.unwords . take 10 . T.words . packageModelDescription . snd <$> packageDyn

profileRoute
  :: Profile.Profile
  -> (R FrontendRoute)
profileRoute p = FrontendRoute_Profile :/ (Username (Profile.username p), Nothing)

packageImage
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Text -- Class
  -> Dynamic t Text
  -> m ()
packageImage className imageDyn =
  elDynAttr "img"
    ((\i -> Map.fromList [("src",imgUrl $ Just i),("class",className)]) <$> imageDyn)
    blank