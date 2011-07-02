{-# LANGUAGE OverloadedStrings #-}

module Controllers.Settings where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB
import qualified  Data.Map as M
import            Control.Monad
import            Control.Monad.Trans
import            Control.Applicative
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Word

import            Data.Time.Clock
import            Data.Time.LocalTime
import            Data.Time.Format
import            System.Locale (defaultTimeLocale)

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing, mapMaybe, listToMaybe)
import qualified  Data.Bson as B
import            Data.List (sortBy)
import            Data.List.Split
import            System.Random


import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform
import            Data.Text (Text)
import            Text.Templating.Heist

import            Data.Time.Calendar
import            Data.Time.LocalTime
import            Data.Time.Clock

import            Application
import            Lib
import            Mail (mailActivation)
import            Utils
import            Views.Site
import            Views.Entry
import            Views.Result
import            Views.Person
import            Views.History
import            Controllers.Form
import            Controllers.Person
import            Controllers.History
import            Models.Entry
import            Models.Account
import            Models.Person
import            Models.History
import            Models.Site


settingsH :: User -> Application ()
settingsH user = do 
   people <- getHouseTabPeople (authUser user)
   historySplice <- historyPage people 0 user
   today <- liftM localDay $ liftIO getLocalTime
   (heistLocal $ (bindSplices 
    [ ("result",           (renderResult  $ currentResult user))
    , ("totalShares",      textSplice $ T.pack $ show $ getTotalShares today people)
    , ("email-value",      textSplice $ TE.decodeUtf8 $ accountEmail user)
    , ("history",          historySplice)
    , ("historyPage",      textSplice $ "1")
    ])) $ renderHT "settings"
