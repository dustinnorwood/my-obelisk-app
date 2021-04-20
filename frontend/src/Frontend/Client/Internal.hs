{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings   #-}
{-# LANGUAGE PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                                                              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Frontend.Client.Internal where

import Control.Lens
import Reflex

import Control.Applicative  (liftA2)
import Data.Proxy           (Proxy (Proxy))
import Data.Text            (Text)
import Servant.API          ((:<|>) ((:<|>)), (:>), NoContent)
import Servant.Auth         (Auth, JWT)
import Servant.Common.Req   (QParam, Req, headers)
import Servant.Reflex       (BaseUrl (BasePath), SupportsServantReflex)
import Servant.Reflex.Multi (ClientMulti, HasClientMulti (..), ReqResult, clientA)

import Common.Api                        (Api)

import Common.Api.Namespace              (Namespace)
import Common.Api.Packages.Package       (Package)
import Common.Api.Packages.Packages      (Packages)
import Common.Api.Packages.Attributes    (CreatePackage)
import Common.Api.Profiles               (Profile)
import Common.Api.User.Account           (Account)
import Common.Api.User.Update            (UpdateUser)
import Common.Api.Users.Credentials      (Credentials)
import Common.Api.Users.Registrant       (Registrant)

fill :: a -> Getting f (a -> b) b
fill a = to ($ a)

data UsersClient f t m = UsersClient
  { _usersLogin
    :: Dynamic t (f (Either Text (Namespace "user" Credentials)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  , _usersRegister
    :: Dynamic t (f (Either Text (Namespace "user" Registrant)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  }
makeLenses ''UsersClient

data UserClient f t m = UserClient
  { _userCurrent
    :: Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  , _userUpdate
    :: Dynamic t (f (Either Text (Namespace "user" UpdateUser)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  }
makeLenses ''UserClient

data PackageClient f t m = PackageClient
  { _packageGet
    :: Event t ()
    -> m (Event t (f (ReqResult () (Namespace "package" Package))))
  }
makeLenses ''PackageClient

data PackagesClient f t m = PackagesClient
  { _packagesList
    :: Dynamic t (f (QParam Integer))
    -> Dynamic t (f (QParam Integer))
    -> Dynamic t (f [Text])
    -> Dynamic t (f [Text])
    -> Event t ()
    -> m (Event t (f (ReqResult () Packages)))
  , _packagesCreate
    :: Dynamic t (f (Either Text (Namespace "package" CreatePackage)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "package" Package))))
  , _packagesFeed
    :: Dynamic t (f (QParam Integer))
    -> Dynamic t (f (QParam Integer))
    -> Event t ()
    -> m (Event t (f (ReqResult () Packages)))
  , _packagesPackage :: f (Dynamic t (Either Text Text)) -> PackageClient f t m
  }
makeLenses ''PackagesClient

data ProfilesClient f t m = ProfilesClient
  { _profileGet
    :: f (Dynamic t (Either Text Text))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "profile" Profile))))
  }
makeLenses ''ProfilesClient

data TagsClient f t m = TagsClient
  { _tagsAll
    :: Event t ()
    -> m (Event t (f (ReqResult () (Namespace "tags" [Text]))))
  }
makeLenses ''TagsClient

data ApiClient f t m = ApiClient
  { _apiUsers    :: UsersClient f t m
  , _apiUser     :: UserClient f t m
  , _apiPackages :: PackagesClient f t m
  , _apiProfiles :: ProfilesClient f t m
  , _apiTags     :: TagsClient f t m
  }
makeLenses ''ApiClient

getClient
  :: forall f t m
  .  (Traversable f, Applicative f, SupportsServantReflex t m)
  => ApiClient f t m
getClient = mkClient (pure $ BasePath "/") -- This would be much better if there was a RouteToUrl BackendRoute
  where
    mkClient bp = ApiClient { .. } :: ApiClient f t m
      where
        c :: ClientMulti t m Api f ()
        c = clientA (Proxy :: Proxy Api)  (Proxy :: Proxy m) (Proxy :: Proxy f) (Proxy :: Proxy ()) bp
        apiUsersC :<|> apiUserC :<|> apiPackagesC :<|> apiProfilesC :<|> apiTagsC = c
        _apiUsers = UsersClient { .. }
          where
            _usersLogin :<|> _usersRegister = apiUsersC
        _apiUser = UserClient { .. }
          where
            _userCurrent :<|> _userUpdate = apiUserC
        _apiPackages = PackagesClient { .. }
          where
            _packagesList :<|> _packagesCreate :<|> _packagesFeed :<|> packageC = apiPackagesC
            _packagesPackage slug = PackageClient { .. }
              where
                _packageGet = packageC slug
        _apiProfiles = ProfilesClient { .. }
          where
            _profileGet = apiProfilesC
        _apiTags = TagsClient { .. }
          where
            _tagsAll = apiTagsC