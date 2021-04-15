{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import           Prelude hiding (id, (.))
import           Control.Category

import           Data.Text (Text)
import           Data.Functor.Identity

import           Obelisk.OAuth.Authorization   (OAuth, oauthRouteEncoder)
import           Obelisk.Route
import           Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName
  BackendRoute_OAuth :: BackendRoute (R OAuth)
  BackendRoute_GetSearchExamples :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api     -> PathSegment "api" $ id
      BackendRoute_OAuth   -> PathSegment "oauth" oauthRouteEncoder
      BackendRoute_GetSearchExamples -> PathSegment "get-search-examples" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
  )

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
