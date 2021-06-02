{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Frontend.Client where

import Control.Lens
import Reflex
import Reflex.Dom.Core hiding (Namespace)

import Common.Api.Packages.Package (PackageModel)
import Control.Monad           (join)
import Data.Aeson              (decode, FromJSON)
import Data.Dependent.Sum      (DSum(..))
import Data.Functor.Identity   (Identity (..))
import Data.Map.Strict         (Map)
import Data.Text               (Text)
import Data.Text.Lazy          (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Reflex.Dom.Xhr          (xhrResponse_responseText, xhrResponse_status)
import Servant.API             (NoContent)
import Servant.Common.Req      (QParam)
import Servant.Reflex.Multi    (ReqResult (ResponseFailure), reqSuccess)

import           Common.Api.Errors                 (ErrorBody)
import           Common.Api.Namespace              (Namespace)
import           Common.Api.Packages.Package       (Package)
import           Common.Api.Packages.Packages      (Packages)
import           Common.Api.Packages.Attributes    (CreatePackage)
import           Common.Api.Profiles               (Profile)
import           Common.Api.User.Account           (Account)
import           Common.Api.User.Update            (UpdateUser)
import           Common.Api.Users.Credentials      (Credentials)
import           Common.Api.Users.Registrant       (Registrant)
import           Common.Api.Validation             (ValidationErrors)
import           Common.Route
import           Frontend.Client.Internal
import           Obelisk.Route                     hiding (decode)

type ClientRes t a = (Event t a, Event t ClientError, Dynamic t Bool)

data ClientError
  = Forbidden
  | NotFound
  | Unauthorised
  | FailedValidation (Maybe (ErrorBody ValidationErrors))
  | OtherError Word Text
  deriving (Show)

urlGET ::
  (MonadHold t m, PostBuild t m, Prerender js t m, FromJSON b) =>
  Dynamic t Text -> m (Event t (Maybe b))
urlGET urlDyn = fmap switchDyn $ prerender (pure never) $ do
  pb <- getPostBuild
  getAndDecode $ leftmost [updated urlDyn, tagPromptlyDyn urlDyn pb]

backendGET ::
  (MonadHold t m, PostBuild t m, Prerender js t m, FromJSON b) =>
  Dynamic t (DSum BackendRoute Identity) -> m (Event t (Maybe b))
backendGET routeDyn = urlGET urlDyn
  where
    urlDyn = renderBackendRoute enc <$> routeDyn
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder

login
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "user" Credentials))
  -> Event t ()
  -> m (ClientRes t (Namespace "user" Account))
login credDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiUsers . usersLogin . fillIdF credDyn . fill submitE
  wireClientRes submitE resE

register
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "user" Registrant))
  -> Event t ()
  -> m (ClientRes t (Namespace "user" Account))
register regDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiUsers . usersRegister . fillIdF regDyn . fill submitE
  wireClientRes submitE resE

getCurrentUser
  :: (Reflex t, Applicative m, Prerender js t m)
  => Event t ()
  -> m (ClientRes t (Namespace "user" Account))
getCurrentUser submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiUser . userCurrent . fill submitE
  wireClientRes submitE resE

updateCurrentUser
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "user" UpdateUser))
  -> Event t ()
  -> m (ClientRes t (Namespace "user" Account))
updateCurrentUser updateDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiUser . userUpdate . fillIdF updateDyn . fill submitE
  wireClientRes submitE resE

getProfile
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (ClientRes t (Namespace "profile" Profile))
getProfile usernameDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiProfiles . profileGet . fillId usernameDyn . fill submitE
  wireClientRes submitE resE

-- TODO FollowUser
-- TODO UnFollowUser

-- TODO Favorite / Unfavorite

allTags
  :: (Reflex t, Applicative m, Prerender js t m)
  => Event t ()
  -> m (ClientRes t (Namespace "tags" [Text]))
allTags submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiTags . tagsAll . fill submitE
  wireClientRes submitE resE

listPackages
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (QParam Integer)
  -> Dynamic t (QParam Integer)
  -> Dynamic t [Text]
  -> Dynamic t [Text]
  -> Event t ()
  -> m (ClientRes t (Map Text PackageModel))
listPackages limitDyn offsetDyn tagsDyn favoritedsDyn submitE =
  fmap switchClientRes $ prerender (pure emptyClientRes) $ do
    resE <- unIdF $ getClient ^. apiPackages . packagesList
      . fillIdF limitDyn
      . fillIdF offsetDyn
      . fillIdF tagsDyn
      . fillIdF favoritedsDyn
      . fill submitE
    wireClientRes submitE resE

packageFeed
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (QParam Integer)
  -> Dynamic t (QParam Integer)
  -> Event t ()
  -> m (ClientRes t Packages)
packageFeed limitDyn offsetDyn submitE =
  fmap switchClientRes $ prerender (pure emptyClientRes) $ do
    resE <- unIdF $ getClient ^. apiPackages . packagesFeed
      . fillIdF limitDyn
      . fillIdF offsetDyn
      . fill submitE
    wireClientRes submitE resE

getPackage
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (ClientRes t (Namespace "package" Package))
getPackage slugDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiPackages . packagesPackage . fillId slugDyn . packageGet . fill submitE
  wireClientRes submitE resE

createPackage
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "package" CreatePackage))
  -> Event t ()
  -> m (ClientRes t (Namespace "package" Package))
createPackage createDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiPackages . packagesCreate . fillIdF createDyn . fill submitE
  wireClientRes submitE resE

-- TODO Update Package
-- TODO Delete Package

-- Helpers ---------------------------------------------------------------------------------------------------

emptyClientRes :: Reflex t => (Event t a, Event t ClientError, Dynamic t Bool)
emptyClientRes = (never, never, constDyn False)

wireClientRes
  :: (Reflex t, MonadHold t m)
  => Event t b
  -> Event t (ReqResult () a)
  -> m (ClientRes t a)
wireClientRes submitE resE = do
  let successE = fmapMaybe reqSuccess resE
  let errorE   = fmapMaybe reqClientError resE
  submittingDyn <- holdDyn False $ leftmost [True <$ submitE, False <$ errorE, False <$ successE]

  pure (successE, errorE, submittingDyn)

reqClientError :: ReqResult tag a -> Maybe ClientError
reqClientError (ResponseFailure _ msg xhrR) = Just $ case view xhrResponse_status xhrR of
  401 -> Unauthorised
  403 -> Forbidden
  404 -> NotFound
  422 -> FailedValidation (xhrR ^? xhrResponse_responseText . _Just . to fromStrict . to encodeUtf8 . to decode . _Just)
  w   -> OtherError w msg
reqClientError _                              = Nothing

switchClientRes
  :: Reflex t
  => Dynamic t (Event t a, Event t b, Dynamic t d)
  -> (Event t a, Event t b, Dynamic t d)
switchClientRes d =
  ( switchDyn . fmap (^. _1) $ d
  , switchDyn . fmap (^. _2) $ d
  , d >>= (^. _3)
  )

switchHoldThroughClientRes
  :: (Reflex t, MonadHold t m)
  => Event t (ClientRes t a)
  -> m (ClientRes t a)
switchHoldThroughClientRes res = do
  successE <- switchHold never (view _1 <$> res)
  failureE <- switchHold never (view _2 <$> res)
  submittingDyn <- fmap join $ holdDyn (constDyn True) (view _3 <$> res)
  pure (successE, failureE, submittingDyn)

unIdF :: (Reflex t, Functor m) => m (Event t (Identity a)) -> m (Event t a)
unIdF = fmap (fmap runIdentity)

idF :: Functor f => f a -> f (Identity a)
idF = fmap Identity

fillId :: a -> Getting f (Identity a -> b) b
fillId a = fill (Identity a)

fillIdF :: Functor f => f a -> Getting g (f (Identity a) -> b) b
fillIdF a = fill (idF a)
