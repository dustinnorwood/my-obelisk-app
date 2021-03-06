{-# LANGUAGE GADTs, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, TypeApplications, DataKinds, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Frontend.Login where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Foldable          (traverse_)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Obelisk.Generated.Static
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Api.Users.Credentials (Credentials (Credentials))
import           Common.Api.User.Account
import           Common.Route
import qualified Frontend.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass)

login
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , MonadHold t m
     , MonadFix m
     , AsFrontendEvent e
     , HasLoggedInAccount s
     , HasFrontendState t s m
     )
  => m ()
login = noUserWidget $ elClass "div" "auth-page" $ do
  elClass "div" "container-page" $ do
    elClass "div" "row" $ do
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ mdo
        ssoHrefsE <- fmap switchDyn . prerender (pure never) $ do
          pbE <- getPostBuild
          mSsoHrefs <- Client.backendGET $ constDyn (BackendRoute_OAuth :/ OAuthProviderRoute_List :/ ())
          pure $ fromMaybe Map.empty <$> mSsoHrefs
        ssoHrefs <- holdDyn Map.empty ssoHrefsE
        elClass "h1" "text-xs-center" $ text "Sign in"

        -- Put a link here that goes to signup
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Register :/ ()) $ text "Need an account?"

        errorDyn <- holdDyn Nothing $ leftmost [Nothing <$ submitE, Just <$> errorE]

        elClass "ul" "error-messages" $
          void $ dyn $ ffor errorDyn $ traverse_ $ \_ ->
            el "li" (text "Login Failed")

        let fbHref = fromMaybe "#" . Map.lookup ("Facebook" :: Text) <$> ssoHrefs
            fbMap = ("href" =:) <$> fbHref
        elDynAttr "a" fbMap . void $
          elAttr "img" ("src" =: (static @"fb.png")
                       <> "width" =: "400"
                       ) $ blank

        let gHref = ("href" =:) . fromMaybe "#" . Map.lookup "Google" <$> ssoHrefs
        elDynAttr "a" gHref . void $
          elAttr "img" ("src" =: (static @"google.png")
                       <> "width" =: "400"
                       ) $ blank

        -- A form for two inputs
        (submitE, successE, errorE) <- el "form" $ mdo
          emailI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ])
          passI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                , ("type","password")
                ])
          -- And a submit button. Not really a submit element. Should fix this
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" submittingDyn $ text "Sign in"

          -- We take the Dynamics from the two inputs and make a Dynamic t Credentials
          -- Dynamic has an applicative instance.
          let credentials = Credentials
                <$> emailI ^. to _inputElement_value
                <*> passI ^. to _inputElement_value

          -- Do a backend call with the Dynamic t Credentials and the Submit Click
          (successE,errorE,submittingDyn) <- Client.login (pure . Namespace <$> credentials) submitE

          pure (submitE, successE, errorE)

        -- When we have a success fire off, we fire a login up the state tree
        -- This sets the JWT token into our state and local storage
        tellEvent $ pure . (_LogIn #) . getToken . token . unNamespace <$> successE

  pure ()
