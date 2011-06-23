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
import            Models.Person
import            Models.Site


entriesH :: User -> Application ()
entriesH user = do 
   {-liftIO $ putStrLn $ show $ length entries-}
   peopleSplice <- getPeopleSplices (authUser user)
   people <- getHouseTabPeople (authUser user)
   entriesSplice <- entriesPage people 0 user
   now <- liftIO $ getCurrentTime
   zone <- liftIO $ getCurrentTimeZone
   let today = localDay $ utcToLocalTime zone now
   (heistLocal $ (bindSplices 
    ([ ("result",           (renderResult  $ currentResult user))
     , ("totalSpent",       textSplice $ T.pack $ moneyShow $ getTotalSpent user)
     , ("currentDateLong",  textSplice $ T.pack $ formatTime defaultTimeLocale "%e %B %Y" today)
     -- the following should be the default until overridden by digestive-functors validation
     , ("date-value",       textSplice $ T.pack $ formatTime defaultTimeLocale "%-m.%d.%Y" today)
     , ("entries",          entriesSplice)
     , ("entriesPage",      textSplice $ "1")
     , ("accountName",      textSplice $ TE.decodeUtf8 (accountName user))
     ] ++ peopleSplice))) $ renderHT "entries"

entriesPageH :: User -> Application ()
entriesPageH user = do 
   page <- getParam "page"
   case page >>= (maybeRead . B8.unpack) of
     Nothing -> mzero
     Just n -> do
       peopleSplice <- getPeopleSplices (authUser user)
       people <- getHouseTabPeople (authUser user)
       entriesSplice <- entriesPage people n user
       (heistLocal $ (bindSplices ((splices entriesSplice) ++ peopleSplice))) $ renderHT "entries/page"
         where splices es = [ ("entries", es)
                            , ("entriesPage", textSplice $ T.pack $ show (n + 1))
                            ]


entriesPage :: [Person] -> Word32 -> User -> Application (Splice Application)
entriesPage ps n user = do entries <- getHouseTabEntries n (authUser user)
                           return (renderEntries ps entries)



addEntry :: User -> Application ()
addEntry user = do
         let mbhtid = userId $ authUser user
         when (isNothing mbhtid) $ redirect "/"
         let (UserId htid) = fromJust mbhtid
         r <- eitherSnapForm (entryForm Nothing) "add-entry-form"
         peopleSplices <- getPeopleSplices (authUser user)
         case r of
             Left splices' -> do
               heistLocal (bindSplices (splices' ++ peopleSplices)) $ renderHT "entries/add"
             Right entry' -> do
               case bs2objid htid of 
                 Nothing -> renderHT "entries/add_failure"                
                 Just h -> do
                   people <- getHouseTabPeople (authUser user)
                   saveHouseTabEntry $ entry' { eHTId = h }
                   nu <- recalculateTotals user
                   entriesSplice <- entriesPage people 0 nu
                   people <- getPeopleSplices (authUser nu)
                   heistLocal (bindSplices ([("entries",entriesSplice),("result",(renderResult  $ currentResult nu))] ++ peopleSplices)) $
                      renderHT "entries/add_success"                
 
editEntry :: User -> Application ()
editEntry user = 
  do i <- getParam "id"
     let mbhtid = userId $ authUser user
     when (isNothing mbhtid) $ redirect "/entries"
     let (UserId htid) = fromJust mbhtid
     case i >>= bs2objid of
      Just eid -> do
        peopleSplices <- getPeopleSplices (authUser user)
        entry <- getHouseTabEntry eid
        r <- eitherSnapForm (entryForm entry) "edit-entry-form" 
        case r of
          Left splices' -> 
            heistLocal (bindSplices (splices' ++ peopleSplices)) $ renderHT "entries/edit"
          Right entry' -> do
            case bs2objid htid of 
               Nothing -> redirect "/entries"               
               Just h -> do
                 people <- getHouseTabPeople (authUser user)
                 let newentry = entry' { eHTId = h, eId = Just eid }
                 saveHouseTabEntry newentry
                 nu <- recalculateTotals user
                 heistLocal (bindSplices (renderEntry people newentry ++ peopleSplices ++ [("result",(renderResult  $ currentResult nu))])) $ renderHT "entries/edit_success"
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
                nu <- recalculateTotals user
                heistLocal (bindSplices [("index", (textSplice . TE.decodeUtf8 . objid2bs) eid),("result",(renderResult  $ currentResult nu))]) $ 
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

