{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Frontend.PackagePreview where


import Reflex.Dom.Core

import           Control.Monad          (join)
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

import Common.Route
import qualified Frontend.Client        as Client
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
     , MonadSample t m
     , Prerender js t m
     )
  => Dynamic t (Map Text PackageModel, Maybe Text)
  -> m ()
packagesPreview artMapDyn = dyn_ $ loaded <$> artMapDyn
  where
    loaded (m, b) = elClass "div" "row" $
        if Map.null m
        then blank
        else void $ list ((\(m, b) -> Map.mapWithKey (\k v -> (k, v, b)) m) <$> artMapDyn) $ packagePreview

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
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender js t m
     )
  => Dynamic t (Text, PackageModel, Maybe Text) -> m ()
packagePreview packageUserDyn' = elClass "div" "package-preview col-md-3" $ do
  elClass "div" "package-meta" $ mdo
    packageUserDyn <- fmap join $ foldDyn (\pm d -> fmap (\(s,_,mt) -> (s,pm,mt)) d) packageUserDyn' packageE
    let packageD = (\(s,pm,_) -> (s,pm)) <$> packageUserDyn
        didIAlreadyFavoriteIt = do
          (_, PackageModel{..}, mUser) <- packageUserDyn
          pure $ ffor mUser $ \t -> t `S.member` packageModelFavorited
        bClass = ffor didIAlreadyFavoriteIt $ \case
          Nothing -> "disabled "
          Just True -> "selected "
          Just False -> ""
    packageImage "" $ packageModelImage . snd <$> packageD
    let dAttrs = (\b -> ("class" =: (b <> "btn btn-outline-primary btn-sm pull-xs-right"))) <$> bClass
    (r,_) <- elDynAttr' "button" dAttrs $ do
      elClass "i" "ion-heart" blank
      text " "
      dynText $ T.pack . show . S.size . packageModelFavorited . snd <$> packageD
    let favoriteE = tag ((,) <$> (fst <$> current packageD) <*> (current didIAlreadyFavoriteIt)) $ domEvent Click r
    let urlE = ffor favoriteE $ \(s, b) -> case b of
          Just True -> BackendRoute_Api :/ ApiRoute_Package :/ (DocumentSlug s, Just (PackageRoute_Unfavorite :/ ()))
          _ -> BackendRoute_Api :/ ApiRoute_Package :/ (DocumentSlug s, Just (PackageRoute_Favorite :/ ()))
    mPackageE <- Client.backendPostEvent ((,()) <$> urlE)
    let packageE = fmapMaybe id mPackageE
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Package :/ (DocumentSlug $ fst a)) <$> packageD)
      $ do
        el "h3" $ dynText $ packageModelTitle . snd <$> packageD
    el "p" $ dynText $ (<>"...") . T.unwords . take 10 . T.words . packageModelDescription . snd <$> packageD
  pure ()

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