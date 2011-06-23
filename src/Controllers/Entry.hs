{-# LANGUAGE OverloadedStrings #-}

module Controllers.Entry where

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
import            Controllers.Form
import            Controllers.Person
import            Models.Entry
import            Models.Account

entriesH :: User -> Application ()
entriesH user = do 
   entriesSplice <- entriesPage 0 user
   {-liftIO $ putStrLn $ show $ length entries-}
   people <- getPeopleSplices (authUser user)
   now <- liftIO $ getCurrentTime
   zone <- liftIO $ getCurrentTimeZone
   (heistLocal $ (bindSplices 
    ([ ("result",  (renderResult  $ currentResult user))
     , ("totalSpent", textSplice $ T.pack $ moneyShow $ getTotalSpent user)
     , ("currentDate", textSplice $ T.pack $ showTime zone now)
     , ("entries", entriesSplice)
     , ("entriesPage", textSplice $ "1")
     , ("accountName", textSplice $ TE.decodeUtf8 (accountName user))
     ] ++ people))) 
     $ renderHT "entries"
     where showTime zone now = formatTime defaultTimeLocale "%e %B %Y" $ localDay $ utcToLocalTime zone now

entriesPageH :: User -> Application ()
entriesPageH user = do 
   page <- getParam "page"
   case page >>= (maybeRead . B8.unpack) of
     Nothing -> mzero
     Just n -> do
       entriesSplice <- entriesPage n user
       people <- getPeopleSplices (authUser user)
       (heistLocal $ (bindSplices ((splices entriesSplice) ++ people))) $ renderHT "entries/page"
         where splices es = [ ("entries", es)
                            , ("entriesPage", textSplice $ T.pack $ show (n + 1))
                            ]


entriesPage :: Word32 -> User -> Application (Splice Application)
entriesPage n user = do entries <- getHouseTabEntries n (authUser user)
                        return (renderEntries entries)



addEntry :: User -> Application ()
addEntry user = do
         let mbhtid = userId $ authUser user
         when (isNothing mbhtid) $ redirect "/"
         let (UserId htid) = fromJust mbhtid
         r <- eitherSnapForm (entryForm Nothing) "add-entry-form"
         people <- getPeopleSplices (authUser user)
         case r of
             Left splices' -> do
               heistLocal (bindSplices (splices' ++ people)) $ renderHT "entries/add"
             Right entry' -> do
               case bs2objid htid of 
                 Nothing -> renderHT "entries/add_failure"                
                 Just h -> do
                   saveHouseTabEntry $ entry' { eHTId = h }
                   recalculateTotals user
                   entries <- getHouseTabEntriesAll (authUser user)
                   people <- getPeopleSplices (authUser user)
                   heistLocal (bindSplices ([("entries",(renderEntries entries))] ++ people)) $
                      renderHT "entries/add_success"                
 
editEntry :: User -> Application ()
editEntry user = 
  do i <- getParam "id"
     let mbhtid = userId $ authUser user
     when (isNothing mbhtid) $ redirect "/entries"
     let (UserId htid) = fromJust mbhtid
     case i >>= bs2objid of
      Just eid -> do
        people <- getPeopleSplices (authUser user)
        entry <- getHouseTabEntry eid
        r <- eitherSnapForm (entryForm entry) "edit-entry-form" 
        case r of
          Left splices' -> 
            heistLocal (bindSplices (splices' ++ people)) $ renderHT "entries/edit"
          Right entry' -> do
            case bs2objid htid of 
               Nothing -> redirect "/entries"               
               Just h -> do
                 let newentry = entry' { eHTId = h, eId = Just eid }
                 saveHouseTabEntry newentry
                 recalculateTotals user
                 heistLocal (bindSplices (renderEntry newentry ++ people)) $ renderHT "entries/show"
      Nothing -> redirect "/entries"

deleteEntry :: User -> Application ()
deleteEntry user = 
  do i <- getParam "id"
     case i >>= bs2objid of
      Just eid -> do
        entry <- getHouseTabEntry eid
        r <- eitherSnapForm (deleteForm entry) "delete-entry-form" 
        case r of
          Left splices' -> 
            heistLocal (bindSplices splices') $ renderHT "entries/delete"
          Right _ -> do
            case (entry, entry >>= eId)  of
              (Just e, Just eid) -> do
                deleteHouseTabEntry e
                recalculateTotals user
                heistLocal (bindSplice "index" $ (textSplice . TE.decodeUtf8 . objid2bs) eid) $ 
                  renderHT "entries/delete_success"
              _ -> redirect "/entries"
              
      Nothing -> redirect "/entries"

entryForm :: Maybe HouseTabEntry -> SnapForm Application Text HeistView HouseTabEntry
entryForm e = mkEntry
    <$> input "id"  (lMO (e >>= eId))
    <*> input "by"  (lMO (liftM eWho e)) `validate` onePerson  `transform` mongoObjectId     <++ errors
    <*> input "for" (lMO' eWhopays e) `validate` manyPeople `transform` mongoObjectIdMany <++ errors 
    <*> inputRead "ammount" "Invalid ammount" (liftM eHowmuch e)  <++ errors 
    <*> input "what" (lm8 eWhat e)   <++ errors 
    <*> input "category" (lm8 eCategory e) `validate` isCategory   <++ errors 
    <*> inputRead "date" "Date must be YYYY.MM.DD like 2011.6.21" (liftM eWhen e)     <++ errors 
  where mkEntry i b f a wha cat whe = HouseTabEntry (bs2objid $ B8.pack i) emptyObjectId b (B8.pack wha) (B8.pack cat) whe a f
        lMO = liftM (B8.unpack . objid2bs)
        lMO' f = liftM (B8.unpack . B8.intercalate "," . map objid2bs . f)
        lm8 f = liftM (B8.unpack . f)



deleteForm :: Maybe HouseTabEntry -> SnapForm Application Text HeistView ()
deleteForm e = const ()
    <$> input "id" (fmap (B8.unpack . objid2bs) $ e >>= eId)

