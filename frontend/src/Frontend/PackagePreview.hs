{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
module Frontend.PackagePreview where


import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)

import Common.Route   (DocumentSlug (..), FrontendRoute (..), Username (..))
import Frontend.Utils (imgUrl, routeLinkDyn, routeLinkDynClass)

import           Common.Api.Packages.Package  (Package)
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
  -> Dynamic t Packages
  -> m ()
packagesPreview artsLoading artsDyn = void . dyn . ffor artsLoading $ bool loaded loading
  where
    loaded =
      let artMapDyn = Map.fromList . fmap (\a -> (Package.id a, a)) . Packages.packages <$> artsDyn
      in void . elClass "div" "row" $ dyn $ ffor artMapDyn $ \m ->
        if Map.null m
        then blankPreview "There is nothing here ... yet!"
        else void $ list artMapDyn packagePreview
    loading = blankPreview "Loading..."
    blankPreview = elClass "div" "package-preview" . el "em" . text

packagePreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => Dynamic t Package -> m ()
packagePreview packageDyn = elClass "div" "package-preview col-md-4" $ do
  elClass "div" "package-meta" $ do
    packageImage "" $ Package.image <$> packageDyn
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      display $ Package.favoritesCount <$> packageDyn
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Package :/ (DocumentSlug $ Package.slug a)) <$> packageDyn)
      $ do
        el "h3" $ dynText $ Package.title <$> packageDyn
        elDynAttr "img" (Map.singleton "href" . Package.image <$> packageDyn) blank
        el "p" $ dynText $ Package.description <$> packageDyn
        el "span" $ text "Read more..."

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