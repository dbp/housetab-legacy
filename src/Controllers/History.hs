{-# LANGUAGE OverloadedStrings #-}

module Controllers.History where

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
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
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

import            Application
import            State
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
import            Models.Entry
import            Models.Account
import            Models.Person
import            Models.History
import            Models.Site


historyH :: User -> Application ()
historyH user = do 
   peopleSplice <- getPeopleSplices (authUser user)
   people <- getHouseTabPeople (authUser user)
   historySplice <- historyPage people 0 user
   (heistLocal $ (bindSplices 
    ([ ("history",          historySplice)
     , ("historyPage",      textSplice $ "1")
     , ("accountName",      textSplice $ TE.decodeUtf8 (accountName user))
     ] ++ peopleSplice))) $ renderHT "history"

historyPageH :: User -> Application ()
historyPageH user = do 
   page <- getParam "page"
   case page >>= (maybeRead . B8.unpack) of
     Nothing -> mzero
     Just n -> do
       peopleSplice <- getPeopleSplices (authUser user)
       people <- getHouseTabPeople (authUser user)
       historySplice <- historyPage people n user
       (heistLocal $ (bindSplices ([ ("history", historySplice)
                                   , ("historyPage", textSplice $ T.pack $ show (n + 1))
                                   ] ++ peopleSplice))) $ renderHT "history/page"


historyPage :: [Person] -> Word32 -> User -> Application (Splice Application)
historyPage ps n user = do hs <- getHistory n (authUser user)
                           return (renderHistory ps hs)
