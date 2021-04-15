{-# LANGUAGE DataKinds, TypeOperators #-}
module Common.Api.Tags (TagsApi) where

import Data.Text   (Text)
import Servant.API (Get, JSON)

import Common.Api.Namespace (Namespace)

type TagsApi = Get '[JSON] (Namespace "tags" [Text])
