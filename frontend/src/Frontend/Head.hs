{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Frontend.Head where

import Reflex.Dom.Core

import           Data.Char     (isSpace)
import           Data.Foldable (fold, traverse_)
import qualified Data.Map      as Map
import           Data.Text     (Text, pack)
import qualified Data.Text     as T

import Obelisk.Generated.Static

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

styleLink' :: DomBuilder t m => Text -> Text -> m ()
styleLink' href integrity =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css"),("integrity",integrity),("crossorigin","anonymous")]) blank

scriptLink :: DomBuilder t m => Text -> m ()
scriptLink src = flip (elAttr "script") blank $ "src" =: src

scriptLink' :: DomBuilder t m => Text -> Text -> m ()
scriptLink' src integrity = flip (elAttr "script") blank $ Map.fromList
  [ ("src", src)
  , ("integrity", integrity)
  , ("crossorigin", "anonymous")
  ]

data FontType = Light | LightItalic | Regular | SemiBold | SemiBoldItalic | Bold | BoldItalic deriving (Eq, Ord, Enum, Show)

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Vaycon"
  -- these are not the typesafe links so that the fonts load relatively to the css.
  elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1, shrink-to-fit=no") $ blank

  -- stylesheet links -------------------------------------------

  styleLink "/static/ionicons/css/ionicons.min.css"
  elAttr "style" ("type"=:"text/css") $ traverse_ (uncurry gfontsFontFamily)
    [ ("Merriweather Sans",[Regular, Bold])
    , ("Source Sans Pro",[Light .. BoldItalic])
    , ("Source Serif Pro",[Regular, Bold])
    , ("Titillium Web",[Regular, Bold])
    ]
  styleLink' "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
             "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO"
  
  -- script links -----------------------------------------------

  -- Auth0 link
  scriptLink "https://cdn.auth0.com/js/auth0-spa-js/1.13/auth0-spa-js.production.js"

  -- Bootstrap script links
  scriptLink' "https://code.jquery.com/jquery-3.3.1.slim.min.js"
              "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
  el "script" $
    text "window.jQuery || document.write('<script src=\"../../assets/js/vendor/jquery-slim.min.js\"><\\/script>')"
  scriptLink' "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
              "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
  scriptLink' "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
              "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
  --styleLink (static @"main.css")

gfontsFontFamily :: DomBuilder t m => Text -> [FontType] -> m ()
gfontsFontFamily ffName = traverse_ (gfontsFontFace ffName)

-- This would be better done with Clay
gfontsFontFace :: DomBuilder t m => Text -> FontType -> m ()
gfontsFontFace familyName fontType = traverse_ text
  [ "@font-face {"
  , "  font-family: '" <> T.toLower familyName <> "';"
  , "  font-style: " <> fontStyle <> ";"
  , "  font-weight: " <> fontWeight <> ";"
  , fold
    [ "  src: url(/static/gfonts/",snaked,"/",noSpaces,"-",styleName,".ttf)"
    , ";"
    ]
  , "} "
  ]
  where
    styleName = pack $ show fontType
    fontWeight = case fontType of
      Light          -> "300"
      LightItalic    -> "300"
      Regular        -> "400"
      SemiBold       -> "600"
      SemiBoldItalic -> "600"
      Bold           -> "700"
      BoldItalic     -> "700"
    fontStyle = case fontType of
      LightItalic    -> "italic"
      SemiBoldItalic -> "italic"
      BoldItalic     -> "italic"
      _              -> "normal"

    noSpaces = T.filter (not . isSpace) familyName
    snaked   = T.replace " " "_" familyName
