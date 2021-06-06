{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                #-}

module Frontend where

import Control.Lens
import Reflex.Dom.Core hiding (Namespace)

import Control.Monad.Trans.Reader (mapReaderT)
import Data.List.NonEmpty         (NonEmpty)
import Data.Monoid                (appEndo)
import Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import Obelisk.Route.Frontend     (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, mapRoutedT, subRoute_)

import Common.Route
import Common.Facebook.Types.Auth
import Common.Types
import Control.Monad
import Data.Functor.Identity (Identity)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Element (scrollIntoView)
import GHCJS.DOM.Window (scrollTo)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom.Core


import           Common.Api.Namespace            (unNamespace)
import qualified Common.Api.User.Account         as Account
import           Common.Route                    (FrontendRoute (..))
import qualified Data.Text as T
import qualified Frontend.Client                 as Client
import           Frontend.FrontendStateT
import           Frontend.Head                   (htmlHead)
import           Frontend.HomePage               (homePage)
import           Frontend.Login                  (login)
import           Frontend.Nav                    (nav)
import           Frontend.Package                (package)
import           Frontend.Profile                (profile)
import           Frontend.Register               (register)
import           Frontend.Search                 (searchPage)
import           Frontend.Settings               (settings)
import           Frontend.Utils                  (pathSegmentSubRoute, routeLinkClass)

type RoutedAppState t m = RoutedT t (R FrontendRoute) (AppState t m)

type AppState t m
  = EventWriterT t
    (NonEmpty FrontendEvent)
    (FrontendStateT t FrontendData m)

 
htmlBody
  :: forall t js m
  . ( ObeliskWidget js t (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mapRoutedT unravelAppState $ do
  mUser <- Client.backendGET . constDyn $ BackendRoute_Api :/ ApiRoute_User :/ ()
  tellEvent $ maybe (pure LogOut) (pure . LogIn) <$> mUser
  mUserDyn <- holdDyn Nothing mUser
  nav
  elAttr "main" ("role"=:"main"<>"class"=:"container") $ subRoute_ pages
  footer
  where
    unravelAppState :: AppState t m () -> m ()
    unravelAppState m = mdo
      lsDyn <- foldDyn appEndo initialFrontendData (foldMap updateFrontendData <$> sE)
      sE <- flip runFrontendStateT lsDyn $ do
        (_, sInnerE) <- runEventWriterT m
        pure sInnerE
      pure ()

    pages 
      :: FrontendRoute a
      -> RoutedT t a
         (EventWriterT t
           (NonEmpty FrontendEvent)
           (FrontendStateT t FrontendData m))
          ()
    pages r = case r of
      FrontendRoute_Home     -> homePage
      FrontendRoute_Search   -> searchPage
      FrontendRoute_Login    -> login
      FrontendRoute_Register -> register
      FrontendRoute_Package  -> package
      FrontendRoute_Settings -> settings
      FrontendRoute_Profile  -> pathSegmentSubRoute profile

footer
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R (FrontendRoute)) m
     , SetRoute t (R (FrontendRoute)) m
     , MonadSample t m
     )
  => m ()
footer = elClass "footer" "footer" $ elClass "div" "container" $ do
  routeLinkClass "logo-font" homeRoute $ text "Vaycon"
  elClass "span" "attribution" $ do
    text "The trillion dollar business opportunity."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend (prerender_ htmlHead htmlHead) htmlBody