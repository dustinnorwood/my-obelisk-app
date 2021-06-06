{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Frontend.Package where

import Reflex.Dom.Core hiding (Element)

import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           JSDOM.Generated.Document (createElement)
import           GHCJS.DOM.Element      (setInnerHTML)
import           GHCJS.DOM.Types        (liftJSM)
import           Obelisk.Route.Frontend
 
import           Common.Api.Packages.Package
import           Common.Api.Namespace              (unNamespace)
import           Common.Route
import qualified Frontend.Client                   as Client
import           Frontend.FrontendStateT
import           Frontend.Utils

package
  :: forall t m js s
  .  ( DomBuilder t m
     , Prerender js t m
     , Routed t DocumentSlug m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
package = elClass "div" "package-page" $ do
  -- We ask our route for the document slug and make the backend call on load
  slugDyn <- askRoute
  pbE <- getPostBuild
  let brDyn = (\s -> BackendRoute_Api :/ ApiRoute_Package :/ (s, Nothing)) <$> slugDyn
  successE <- Client.backendGET brDyn
  elClass "div" "container page" $ do
    widgetHold blank $ maybe blank (uncurry $ packageContent slugDyn) <$> successE
  pure ()

packageContent
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender js t m
     )
  => Dynamic t DocumentSlug
  -> PackageModel
  -> Maybe Text
  -> m ()
packageContent slugDyn packageModel mUser = do
  let html = packageModelBody packageModel
  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "br" blank
      el "br" blank
      el "br" blank
      packageMeta slugDyn packageModel mUser
      elClass "div" "package-container" $ do
        el "h1" $ text $ packageModelTitle packageModel
        el "p" $ text $ packageModelDescription packageModel
        -- We are a little clumsy with dealing with not having
        -- an package. We just disply a blank element while we
        -- dont have one. Should be better. :)
  prerender_ blank $ elClass "div" "row package-content" $ do
    d <- askDocument
    -- We have to sample the initial value to set it on creation
    e <- liftJSM $ do
      -- This wont execute scripts, but will allow users to XSS attack through
      -- event handling javascript attributes in any raw HTML that is let
      -- through the markdown renderer. But this is the simplest demo that
      -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
      -- potentially more robust solution (we could filter out js handler attrs
      -- with something like that).
      -- It's worth noting that the react demo app does exactly what this does:
      -- https://github.com/gothinkster/react-redux-realworld-example-app/blob/master/src/components/Package/index.js#L60
      e <- createElement d ("div" :: String)
      setInnerHTML e html
      pure e
    -- Put out raw element into our DomBuilder
    placeRawElement e

packageMeta
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender js t m
     )
  => Dynamic t DocumentSlug
  -> PackageModel
  -> Maybe Text
  -> m ()
packageMeta slugDyn packageModel mu = elClass "div" "package-meta" $ mdo
  let packageUserDyn' = (\s -> (s, packageModel, mu)) <$> slugDyn
  packageUserDyn <- fmap join $ foldDyn (\pm d -> fmap (\(s,_,mt) -> (s,pm,mt)) d) packageUserDyn' packageE
  let packageD = (\(s,pm,_) -> (s,pm)) <$> packageUserDyn
      didIAlreadyFavoriteIt = do
        (_, PackageModel{..}, mUser) <- packageUserDyn
        pure $ ffor mUser $ \t -> t `S.member` packageModelFavorited
      bClass = ffor didIAlreadyFavoriteIt $ \case
        Nothing -> "disabled "
        Just True -> "selected "
        Just False -> ""
  packageImage "full-size" $ packageModelImage . snd <$> packageD
  let dAttrs = (\b -> ("class" =: (b <> "btn btn-outline-primary btn-sm pull-xs-right"))) <$> bClass
  (r,_) <- elDynAttr' "button" dAttrs $ do
    elClass "i" "ion-heart" blank
    dynText $ ffor didIAlreadyFavoriteIt $ \case
      Nothing -> ""
      Just False -> " Add to Wishlist ("
      Just True -> " Remove from Wishlist ("
    dynText $ T.pack . show . S.size . packageModelFavorited . snd <$> packageD
    dynText $ ffor didIAlreadyFavoriteIt $ \case
      Nothing -> ""
      _ -> ")"
  let favoriteE = tag ((,) <$> (fst <$> current packageD) <*> (current didIAlreadyFavoriteIt)) $ domEvent Click r
  let urlE = ffor favoriteE $ \(s, b) -> case b of
        Just True -> BackendRoute_Api :/ ApiRoute_Package :/ (s, Just (PackageRoute_Unfavorite :/ ()))
        _ -> BackendRoute_Api :/ ApiRoute_Package :/ (s, Just (PackageRoute_Favorite :/ ()))
  packageE <- fmapMaybe Prelude.id <$> (Client.backendPostEvent ((,()) <$> urlE))
  pure ()

packageImage
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Text -- Class
  -> Dynamic t Text
  -> m ()
packageImage className imageDyn =
  elDynAttr "img"
    ((\i -> M.fromList [("src",imgUrl $ Just i),("class",className)]) <$> imageDyn)
    blank