{-# LANGUAGE OverloadedStrings #-}
module Controllers.Form where
  
import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform

import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            State
import            Control.Applicative
import            Control.Monad
import            Text.Templating.Heist

import            Application


nonEmpty :: Validator Application Html String
nonEmpty = check "String must not be empty." $ \s -> not $ null s
    
lenOne :: Validator Application Html String
lenOne = check "String must be a single character." $ \s -> length s == 1

