{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import           Backend.Config
import           Backend.Database
import           Backend.Login
import           Backend.Monad
import           Common.Api                      (api)
import           Common.Api.Packages.Package     (PackageModel(..))
import           Common.Route
import           Control.Exception.Safe          (throwString)
import           Control.Lens                    ((^.))
import           Control.Monad.FT
import           Control.Monad.IO.Class          (liftIO)
import qualified Crypto.JOSE                      as HOSE
import qualified Crypto.JOSE.Types                as HOSE
import           Data.Aeson
import           Data.Dependent.Sum              (DSum ((:=>)))
import           Data.Functor.Identity           (Identity (..))
import           Data.IORef
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import           Obelisk.OAuth.Authorization     (OAuth (..), RedirectUriParams (..))
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.ExecutableConfig.Lookup
import           Obelisk.Route                   hiding (decode, encode)
import           Prelude                         hiding (lookup)
import "servant-snap" Servant                    (serveSnap)
import           Snap

getYolo :: Text -> IO Text
getYolo l = do
  configs <- liftIO $ getConfigs
  let l' = "backend/" <> l
      route = fromMaybe (error . T.unpack $ "Please fill in config: config/backend/" <> l') $
                T.decodeUtf8 <$> M.lookup l' configs
  pure route

runVaycon = runVayconMemServerM . VayconMemServerEnv

pkgs :: M.Map Text PackageModel
pkgs = M.fromList
          [("burger-pursuit", PackageModel
            "Burger Pursuit"
            "Ketchup to these patties in this all-out arms race to the best burger bars in Dallas"
            "/static/packages/burger_pursuit.png"
            "# Things that you will need\n\n- Cake\n- Party Cannon"
            (read "2021-03-27 23:48:45.982494 UTC")
            (read "2021-03-27 23:48:45.982494 UTC")
            (S.fromList ["we-think-you-will-like", "today", "price-med"])
            (S.fromList ["a","b","c"])
          ),
          ("dinos-and-drinks", PackageModel
            "Dinos and Drinks"
            "Bruh... muhfuckin Dinos!"
            "/static/packages/dinos_and_drinks.png"
            "TODO"
            (read "2021-03-27 23:51:44.856755 UTC")
            (read "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["we-think-you-will-like", "this-weekend", "price-med"])
            (S.fromList ["a","b","c","d"])
          ),
          ("animals-of-dallas", PackageModel
            "Animals of Dallas"
            "Do you live under a rock? Come hang with some of your own kind that literally do."
            "/static/packages/animals_of_dallas.png"
            "TODO"
            (read "2021-03-27 23:51:44.856755 UTC")
            (read "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["we-think-you-will-like", "popular", "this-weekend", "price-low"])
            (S.fromList ["a","b","c","d","e"])
          ),
          ("beef-hunt", PackageModel
            "Beef Hunt"
            "Your voices have been herd: all aboard the stock train to Tenderville! (all well-done requests will be met with a condescending guffaw and a lifetime ban from these bovine-centric establishments)"
            "/static/packages/beef_hunt.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["we-think-you-will-like", "popular", "today", "price-high"])
            (S.fromList ["a","c","e"])
          ),
          ("tacos-and-talks", PackageModel 
            "Tacos and Talks"
            "No, this isn't your drunk cousin cooking velveeta and rotel while blathering about their workplace drama. Come listen to certified TEDx speakers blather about _their_ workplace drama while indulging in one of the few things Texas does right."
            "/static/packages/tacos_and_talks.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["top-rated", "price-low"])
            (S.fromList ["c","e"])
          ),
          ("zoo-stuff", PackageModel 
            "Zoo Stuff"
            "We couldn't come up with a better name for this one?"
            "/static/packages/zoo_stuff.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["top-rated", "today", "this-weekend", "price-med"])
            (S.fromList ["x","y","z"])
          ),
          ("girls-girls-girls", PackageModel
            "Girls Girls Girls"
            "There's nothing religious about this Easter Egg! Come party with the best bunnies Dallas has to offer, if that's what you're into."
            "/static/packages/girls_girls_girls.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["top-rated", "price-lambo"])
            (S.fromList ["w","x","y","z"])
          ),
          ("water-world", PackageModel
            "Water World"
            "Basically our Girls Girls Girls package for the whole family."
            "/static/packages/water_world.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["top-rated", "popular", "price-high"])
            S.empty),
          ("tipsy-times", PackageModel
            "Tipsy Time"
            "The best night out north of 6th Street! Don't waste this opportunity to get hammered with your coworkers or by yourself at these bars, where the drinks are cheaper than the people drinking them."
            "/static/packages/tipsy_times.png"
            "TODO"
            (read  "2021-03-27 23:51:44.856755 UTC")
            (read  "2021-03-27 23:51:44.856755 UTC")
            (S.fromList ["top-rated", "popular", "price-low"])
            (S.fromList ["w"])
          )
          ]


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      pgConnStr <- getYolo "pgConnStr"
      jwtKey    <- getYolo "jwtKey"
      let jwk   =
               HOSE.fromKeyMaterial
             . HOSE.OctKeyMaterial
             . HOSE.OctKeyParameters
             . HOSE.Base64Octets
             . T.encodeUtf8
             $  jwtKey
      env <- mkEnv pgConnStr -- jwk
      liftIO $ putStrLn "About to test the db connection. If ob run dies, check out config/backend/pgConnStr"
      -- _ <- openVayconDb (T.encodeUtf8 pgConnStr)

      cfg <- readBackendConfig
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg
      pkgsRef <- newIORef pkgs
      serve $ \case
        BackendRoute_Missing :=> Identity () -> do
          writeLBS "404"
        BackendRoute_OAuth :/ oauthProviderR -> case oauthProviderR of
          OAuthProviderRoute_List :=> Identity () -> do
            let fbLink = mkFBLoginLink cfg . Just $ renderFrontendRoute (_backendConfig_enc cfg) homeRoute
                gLink = mkGoogleLoginLink cfg . Just $ renderFrontendRoute (_backendConfig_enc cfg) homeRoute
                hrefs = M.fromList [("Facebook" :: Text, fbLink), ("Google", gLink)]
            writeLBS $ encode hrefs
          OAuthProviderRoute_FB :=> Identity (OAuth_RedirectUri :=> Identity p) -> case p of
            Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
            Just (RedirectUriParams code mstate) -> do
              handleFBOAuthCallback cfg code
              redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) homeRoute) mstate
          OAuthProviderRoute_Google :=> Identity (OAuth_RedirectUri :=> Identity p) -> case p of
            Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
            Just (RedirectUriParams code mstate) -> do
              handleGoogleOAuthCallback cfg code
              redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) homeRoute) mstate
        BackendRoute_Api :/ apiR  -> do
          eUser <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) homeRoute) >>= \case
            Left e -> pure $ Left e
            Right u -> pure $ Right u
          runVaycon pkgsRef $ case apiR of
            ApiRoute_Users :/ _ -> pure ()
            ApiRoute_User :/ _ -> case eUser of
              Right t -> writeLBS $ encode t
              Left u -> pure ()
            ApiRoute_Packages :/ packagesR -> case packagesR of
              PackagesRoute_Get :/ w -> do
                pkgs <- fromMaybe M.empty <$> select @(M.Map Text PackageModel) w
                writeLBS $ encode (pkgs, either (const Nothing) Just eUser)
              PackagesRoute_Search :/ sp -> do
                pkgs <- maybe M.empty unPrefixed <$> select @(Prefixed (M.Map Text PackageModel)) sp
                writeLBS $ encode pkgs
              PackagesRoute_Feed :/ w -> do
                liftIO . putStrLn $ show w
            ApiRoute_Package :/ (DocumentSlug slug, m) -> case m of
              Nothing -> do
                mPkg <- select @PackageModel slug
                writeLBS $ encode $ (\pkg -> (pkg, either (const False) (const True) eUser)) <$> mPkg
              Just (PackageRoute_Favorite :/ ()) -> do
                case eUser of
                  Left e -> do
                    mPkg <- select @PackageModel slug
                    writeLBS $ encode mPkg
                  Right e -> do
                    insert @Favorite (slug, e) Favorite
                    mPkg <- select @PackageModel slug
                    writeLBS $ encode mPkg
              Just (PackageRoute_Unfavorite :/ ()) -> do
                case eUser of
                  Left e -> do
                    mPkg <- select @PackageModel slug
                    writeLBS $ encode mPkg
                  Right e -> do
                    delete @Favorite (slug, e)
                    mPkg <- select @PackageModel slug
                    writeLBS $ encode mPkg
              _ -> writeLBS "404"
            ApiRoute_Profiles :/ _ -> pure ()
            ApiRoute_Tags :/ _ -> pure ()
  }
