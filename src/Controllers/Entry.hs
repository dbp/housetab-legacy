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
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing, mapMaybe)
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
import            Lib
import            Mail (mailActivation)
import            Utils
import            Views.Site
import            Views.Entry
import            Views.Result
import            Views.Person
import            Controllers.Form
import            Controllers.Person
import            Controllers.Tutorial
import            Models.Entry
import            Models.Account
import            Models.Person
import            Models.History
import            Models.Site


entriesH :: User -> Application ()
entriesH user = do 
   people <- getHouseTabPeople (authUser user)
   entriesSplice <- entriesPage people 0 user
   tutorialStep user "3" "4"
   (heistLocal $ (bindSplices
    ([ ("result",           (renderResult  $ currentResult user))
     , ("totalSpent",       textSplice $ T.pack $ moneyShow $ getTotalSpent user)
     , ("entries",          entriesSplice)
     , ("entriesPage",      textSplice $ "1")
     , ("for-value",        textSplice $ T.intercalate "," $ map (TE.decodeUtf8 . objid2bs) $ mapMaybe pId people)
     ]))) $ renderHT "entries"

entriesPageH :: User -> Application ()
entriesPageH user = do 
   page <- getParam "page"
   case page >>= (maybeRead . B8.unpack) of
     Nothing -> mzero
     Just n -> do
       people <- getHouseTabPeople (authUser user)
       entriesSplice <- entriesPage people n user
       (heistLocal $ (bindSplices (splices entriesSplice))) $ renderHT "entries/page"
         where splices es = [ ("entries", es)
                            , ("entriesPage", textSplice $ T.pack $ show (n + 1))
                            ]


entriesPage :: [Person] -> Word32 -> User -> Application (Splice Application)
entriesPage ps n user = do entries <- getHouseTabEntries n (authUser user)
                           return (renderEntries ps entries)


showEntry :: User -> Application ()
showEntry user = do
  do i <- getParam "id"
     let mbhtid = userId $ authUser user
     when (isNothing mbhtid) $ redirect "/entries"
     let (UserId htid) = fromJust mbhtid
     case i >>= bs2objid of
      Just eid -> do
        mentry <- getHouseTabEntry eid
        people <- getHouseTabPeople (authUser user)
        case mentry of
          Just entry -> heistLocal (bindSplices (renderEntry people entry)) $ renderHT "entries/show"
          _ -> redirect "/entries"
      _ -> redirect "/entries"
               
addEntry :: User -> Application ()
addEntry user = do
         let mbhtid = userId $ authUser user
         when (isNothing mbhtid) $ redirect "/"
         let (UserId htid) = fromJust mbhtid
         r <- eitherSnapForm (entryForm Nothing) "add-entry-form"
         case r of
             Left splices' -> do
               heistLocal (bindSplices splices') $ renderHT "entries/add"
             Right entry' -> do
               case bs2objid htid of 
                 Nothing -> renderHT "entries/add_failure"                
                 Just h -> do
                   people <- getHouseTabPeople (authUser user)
                   saveHouseTabEntry $ entry' { eHTId = h }
                   when (recordHistory user) $ trackAdd $ entry' { eHTId = h }
                   nu <- recalculateTotals user
                   entriesSplice <- entriesPage people 0 nu
                   tutorialStep user "4" "5"
                   heistLocal (bindSplices [("entries",entriesSplice),("result",(renderResult  $ currentResult nu))]) $
                      renderHT "entries/add_success"                
 
editEntry :: User -> Application ()
editEntry user = 
  do i <- getParam "id"
     let mbhtid = userId $ authUser user
     when (isNothing mbhtid) $ redirect "/entries"
     let (UserId htid) = fromJust mbhtid
     case i >>= bs2objid of
      Just eid -> do
        entry <- getHouseTabEntry eid
        r <- eitherSnapForm (entryForm entry) "edit-entry-form" 
        case r of
          Left splices' -> 
            heistLocal (bindSplices splices') $ renderHT "entries/edit"
          Right entry' -> do
            case (bs2objid htid,entry) of 
               (Just h, Just oldentry) -> do
                 people <- getHouseTabPeople (authUser user)
                 let newentry = entry' { eHTId = h, eId = Just eid }
                 saveHouseTabEntry newentry
                 when (recordHistory user) $ trackEdit oldentry newentry
                 nu <- recalculateTotals user
                 heistLocal (bindSplices (renderEntry people newentry ++ [("result",(renderResult  $ currentResult nu))])) $ renderHT "entries/edit_success"
               _ -> redirect "/entries"               
              
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
                when (recordHistory user) $ trackDelete e
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
    <*> inputRead "ammount" "Must be a number, like 20.5" (liftM eHowmuch e)  <++ errors 
    <*> input "what" (lm8 eWhat e)   <++ errors 
    <*> input "category" (lm8 eCategory e) `validate` isCategory   <++ errors 
    <*> inputRead "date" "Date must be MM.DD.YYYY like 6.21.2011" (liftM eWhen e)     <++ errors 
  where mkEntry i b f a wha cat whe = HouseTabEntry (bs2objid $ B8.pack i) emptyObjectId b (B8.pack wha) (B8.pack cat) whe a f
        lMO = liftM (B8.unpack . objid2bs)
        lMO' f = liftM (B8.unpack . B8.intercalate "," . map objid2bs . f)
        lm8 f = liftM (B8.unpack . f)



deleteForm :: Maybe HouseTabEntry -> SnapForm Application Text HeistView ()
deleteForm e = const ()
    <$> input "id" (fmap (B8.unpack . objid2bs) $ e >>= eId)

