{-# LANGUAGE OverloadedStrings #-}

module Views.Account where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import qualified  Data.Map as M
import            Data.List (intercalate)

import            Data.Maybe (fromMaybe, maybeToList)
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.ByteString as BS
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Snap.Extension.Session.CookieSession

import            Control.Monad.Trans (lift)

import            Views.Site
import            Application

tutorialSplice :: Splice Application
tutorialSplice = do node <- getParamNode
                    s <- lift $ getFromSession "tutorial-step"
                    case X.getAttribute "step" node of
                      Just step | Just (TE.encodeUtf8 step) == s -> return (X.elementChildren node)
                      _ -> return []
