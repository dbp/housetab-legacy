{-# LANGUAGE OverloadedStrings #-}

module Common where
  
import Text.Templating.Heist
import Snap.Extension.Heist
import Data.ByteString (ByteString)

import Notification
import Application

renderHT :: ByteString -> Application ()
renderHT = (heistLocal $ (bindSplice "notification" notificationSplice)) . render
