{-# LANGUAGE FlexibleContexts, LambdaCase, MonoLocalBinds, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RecursiveDo, ScopedTypeVariables                                      #-}

module Frontend.Package where

import Reflex.Dom.Core hiding (Element)

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
     , PostBuild t m
     , MonadHold t m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
package = elClass "div" "package-page" $ do
  -- We ask our route for the document slug and make the backend call on load
  slugDyn <- askRoute
  pbE <- getPostBuild
  let brDyn = (\(DocumentSlug s) -> BackendRoute_Api :/ ApiRoute_Package :/ ([s], M.empty)) <$> slugDyn
  successE <- Client.backendGET brDyn

  packageDyn <- holdDyn Nothing successE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "br" blank
      el "br" blank
      el "br" blank
      packageImage "full-size" $ maybe "" packageModelImage <$> packageDyn
      elClass "div" "package-container" $ do
        el "h1" $ dynText $ maybe "" packageModelTitle <$> packageDyn
        el "p" $ dynText $ maybe "" packageModelDescription <$> packageDyn
        -- We are a little clumsy with dealing with not having
        -- an package. We just disply a blank element while we
        -- dont have one. Should be better. :)
        void $ dyn $ maybe blank packageMeta <$> packageDyn
  elClass "div" "container page" $ do
    packageContent packageDyn
    el "hr" blank
    elClass "div" "row package-actions" $
      void $ dyn $ maybe blank packageMeta <$> packageDyn

packageMeta
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => PackageModel
  -> m ()
packageMeta art = elClass "div" "package-meta" $ do
  elClass "div" "info" $ do
    elClass "span" "date" $ text (showText $ packageModelCreatedAt art)
  actions
  where
    actions = do
      -- TODO : Do something with this click
      _ <- buttonClass "btn btn-sm btn-outline-primary action-btn" (constDyn False) $ do
        elClass "i" "ion-heart" blank
        text " Add to Wishlist ("
        elClass "span" "counter" $ text $ showText (S.size $ packageModelFavorited art)
        text ")"
      pure ()

packageContent
  :: forall t m js
  .  ( DomBuilder t m
     , Prerender js t m
     )
  => Dynamic t (Maybe PackageModel)
  -> m ()
packageContent packageDyn = prerender_ (text "Rendering Document...") $ do
  let htmlDyn = (maybe "" packageModelBody) <$> packageDyn
  elClass "div" "row package-content" $ do
    d <- askDocument
    -- We have to sample the initial value to set it on creation
    htmlT <- sample . current $ htmlDyn
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
      setInnerHTML e htmlT
      pure e
    -- And make sure we update the html when the package changes
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated htmlDyn
    -- Put out raw element into our DomBuilder
    placeRawElement e

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