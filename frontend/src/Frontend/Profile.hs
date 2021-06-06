{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms                                                               #-}
module Frontend.Profile where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Bool              (bool)
import Data.Functor           (void)
import Data.Maybe             (fromMaybe)
import qualified Data.Map.Strict as M
import Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)
import Servant.Common.Req     (QParam (QNone))

import           Common.Api.Packages.Packages (Packages (..))
import           Common.Api.Namespace         (Namespace(Namespace))
import qualified Common.Api.Profiles.Profile  as Profile
import           Common.Route
import           Frontend.PackagePreview              (packagesPreview)
import qualified Frontend.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = do
  pbE <- getPostBuild
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          (loadSuccessE,_,_) <- Client.getProfile
            (pure . unUsername <$> usernameDyn)
            (leftmost [pbE,void . updated $ usernameDyn])

          void $ widgetHold (text "Loading") $ ffor loadSuccessE $ \(Namespace acct) -> do
            -- profileImage "user-img" (constDyn $ Profile.image acct)
            el "h4" $ text $ Profile.username acct
            el "p" $ text $ Profile.bio acct
            _ <- buttonClass "btn btn-sm btn-outline-secondary action-btn" (constDyn False) $ do
              elClass "i" "ion-plus-round" blank
              text " Follow "
              text $ Profile.username acct
            pure ()
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "packages-toggle" $ do
          elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Packages"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"

          let rDyn = constDyn rdef
          mPkgsE <- Client.backendGET $ (\r -> BackendRoute_Api :/ ApiRoute_Packages :/ PackagesRoute_Get :/ r) <$> rDyn
          pkgsDyn <- holdDyn (M.empty, Nothing) $ fromMaybe (M.empty, Nothing) <$> mPkgsE
          packagesPreview pkgsDyn
  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
