{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import           Prelude hiding (id, (.))
import           Control.Categorical.Bifunctor (bimap)
import           Control.Category
import           Control.Lens hiding (bimap)
import           Control.Monad (join)

import           Data.Bifunctor (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Functor.Identity
import           Text.Read (readMaybe)

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
  BackendRoute_OAuth :: BackendRoute (R OAuthProviderRoute)
  BackendRoute_GetSearchExamples :: BackendRoute ()

data OAuthProviderRoute :: * -> * where
  OAuthProviderRoute_List   :: OAuthProviderRoute ()
  OAuthProviderRoute_FB     :: OAuthProviderRoute (R OAuth)
  OAuthProviderRoute_Google :: OAuthProviderRoute (R OAuth)

data ApiRoute :: * -> * where
  ApiRoute_Users :: ApiRoute PageName
  ApiRoute_User :: ApiRoute ()
  ApiRoute_Packages :: ApiRoute (R PackagesRoute)
  ApiRoute_Profiles :: ApiRoute PageName
  ApiRoute_Tags :: ApiRoute PageName

data Windowed a = Windowed
  { _limit       :: Maybe Integer
  , _offset      :: Maybe Integer
  , _item        :: a
  } deriving (Eq, Ord, Show)

tshow :: Show a => a -> Text
tshow = T.pack . show

queryEncoder :: (Applicative check, Applicative parse, Read a, Show a) => Encoder check parse (Maybe a) Text
queryEncoder = unsafeMkEncoder $ EncoderImpl
 (pure . readMaybe . T.unpack)
 (tshow)

mkQueryList :: [(Text, Maybe Text)] -> Map Text (Maybe Text)
mkQueryList = M.fromList . catMaybes . map (\(t, m) -> (t,) . Just <$> m)

windowedEncoder :: Encoder Identity Identity           decoded  (Map Text (Maybe Text))
                -> Encoder Identity Identity (Windowed decoded) (Map Text (Maybe Text))
windowedEncoder itemEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> Identity $ Windowed (decode queryEncoder . fromMaybe "" . join $ M.lookup "limit" m)
                             (decode queryEncoder . fromMaybe "" . join $ M.lookup "offset" m)
                             (decode itemEncoder m))
  (\(Windowed l o i) -> M.union (mkQueryList [("limit", tshow <$> l), ("offset", tshow <$> o)]) (encode itemEncoder i))

data GetPackagesParams = GetPackagesParams
  { _tag         :: Maybe Text
  , _favorited   :: Maybe Text
  } deriving (Eq, Ord, Show)

type GetPackages     = Windowed GetPackagesParams
type GetPackagesFeed = Windowed ()

getPackagesParamsEncoder :: (Applicative check, Applicative parse) => Encoder check parse GetPackagesParams (Map Text (Maybe Text))
getPackagesParamsEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> pure $ GetPackagesParams (join $ M.lookup "tag" m) (join $ M.lookup "favorited" m))
  (\(GetPackagesParams t f) -> mkQueryList [("tag", t), ("favorited", f)])

getPackagesEncoder :: Encoder Identity Identity GetPackages (Map Text (Maybe Text))
getPackagesEncoder = windowedEncoder getPackagesParamsEncoder

getPackagesFeedEncoder :: Encoder Identity Identity GetPackagesFeed (Map Text (Maybe Text))
getPackagesFeedEncoder = windowedEncoder . unsafeMkEncoder $ EncoderImpl (pure . const ()) (const M.empty)

data PackagesRoute :: * -> * where
  PackagesRoute_Get  :: PackagesRoute GetPackages
  PackagesRoute_Feed :: PackagesRoute GetPackagesFeed

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
      BackendRoute_Missing           -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api               -> PathSegment "api" $ apiRouteEncoder
      BackendRoute_OAuth             -> PathSegment "oauth" $ oauthProviderRouteEncoder
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
  ApiRoute_User -> PathSegment "user" $ unitEncoder mempty
  ApiRoute_Packages -> PathSegment "packages" $ packagesRouteEncoder
  ApiRoute_Profiles -> PathSegment "profiles" $ id
  ApiRoute_Tags -> PathSegment "tags" $ id

packagesRouteEncoder :: Encoder (Either Text) (Either Text) (R PackagesRoute) PageName
packagesRouteEncoder = pathComponentEncoder $ \case
  PackagesRoute_Get -> PathEnd
                     . hoistParse (pure . runIdentity)
                     $ hoistCheck (pure . runIdentity) getPackagesEncoder
  PackagesRoute_Feed -> PathSegment "feed"
                      $ queryOnlyEncoder .
                      ( hoistParse (pure . runIdentity)
                      . hoistCheck (pure . runIdentity)
                      $ getPackagesFeedEncoder
                      )

oauthProviderRouteEncoder :: Encoder (Either Text) (Either Text) (R OAuthProviderRoute) PageName
oauthProviderRouteEncoder = pathComponentEncoder $ \case
  OAuthProviderRoute_List -> PathSegment "list" $ unitEncoder mempty
  OAuthProviderRoute_FB -> PathSegment "fb" $ oauthRouteEncoder
  OAuthProviderRoute_Google -> PathSegment "google" $ oauthRouteEncoder

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''ApiRoute
  , ''PackagesRoute
  , ''OAuthProviderRoute
  , ''FrontendRoute
  , ''ProfileRoute
  ]
