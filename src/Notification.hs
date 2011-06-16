{-# LANGUAGE OverloadedStrings #-}

{-module Notification where
  

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Snap.Extension.Session.CookieSession
import Text.Templating.Heist
import qualified Text.XmlHtml as X
import Control.Monad.Trans

import Application

setNotification :: BS.ByteString -> Application ()
setNotification msg = setInSession "notification" msg

notificationSplice :: Splice Application
notificationSplice = do msg <- lift $ getFromSession "notification"
                        lift $ deleteFromSession "notification"
                        return $ maybe [X.TextNode ""] ht msg
        where ht ms = [X.Element "div" [("id", "notification")] [X.TextNode $ T.decodeUtf8 ms]]-}