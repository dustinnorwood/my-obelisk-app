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
import           Control.Categorical.Bifunctor (bimap)
import           Control.Category
import           Control.Lens hiding (bimap)

import           Data.Text (Text)
import           Data.Functor.Identity

import           Obelisk.OAuth.Authorization   (OAuth, oauthRouteEncoder)
import           Obelisk.Route
import           Obelisk.Route.TH

newtype DocumentSlug = DocumentSlug { unDocumentSlug :: Text } deriving (Eq, Ord, Show)
makeWrapped ''DocumentSlug
newtype Username = Username { unUsername :: Text } deriving (Eq, Ord, Show)
makeWrapped ''Username

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (R ApiRoute)
  BackendRoute_OAuth :: BackendRoute (R OAuth)
  BackendRoute_GetSearchExamples :: BackendRoute ()

data ApiRoute :: * -> * where
  ApiRoute_Users :: ApiRoute PageName
  ApiRoute_User :: ApiRoute PageName
  ApiRoute_Packages :: ApiRoute PageName
  ApiRoute_Profiles :: ApiRoute PageName
  ApiRoute_Tags :: ApiRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Package :: FrontendRoute DocumentSlug
  FrontendRoute_Profile :: FrontendRoute (Username, Maybe (R ProfileRoute))
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data ProfileRoute :: * -> * where
  ProfileRoute_Favourites :: ProfileRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api     -> PathSegment "api" $ apiRouteEncoder
      BackendRoute_OAuth   -> PathSegment "oauth" oauthRouteEncoder
      BackendRoute_GetSearchExamples -> PathSegment "get-search-examples" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Package -> PathSegment "package" $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_Profile -> PathSegment "profile" $
        let profileRouteEncoder = pathComponentEncoder $ \case
              ProfileRoute_Favourites -> PathSegment "favorites" $ unitEncoder mempty
        in ( pathSegmentEncoder . bimap unwrappedEncoder (maybeEncoder (unitEncoder mempty) profileRouteEncoder ) )

  )

apiRouteEncoder :: Encoder (Either Text) (Either Text) (R ApiRoute) PageName
apiRouteEncoder = pathComponentEncoder $ \case
  ApiRoute_Users -> PathSegment "users" $ id
  ApiRoute_User -> PathSegment "user" $ id
  ApiRoute_Packages -> PathSegment "packages" $ id
  ApiRoute_Profiles -> PathSegment "profiles" $ id
  ApiRoute_Tags -> PathSegment "tags" $ id

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''ApiRoute
  , ''FrontendRoute
  , ''ProfileRoute
  ]
