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

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified  Data.Bson as B
import            Data.List (sortBy)
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
import            Views.Site
import            Views.Entry
import            Views.Result
import            Controllers.Form
import            Controllers.Person
import            Models.Entry
import            Models.Account

entriesH :: User -> Application ()
entriesH user = do 
   entries <- getHouseTabEntries (authUser user)
   (heistLocal $ (bindSplices (splices entries))) $ renderHT "entries"
     where splices es = [ ("result",  (renderResult  $ currentResult user))
                        , ("entries", (renderEntries es))
                        ]


addEntry :: User -> Application ()
addEntry user = do
         let mbhtid = userId $ authUser user
         when (isNothing mbhtid) $ redirect "/"
         let (UserId htid) = fromJust mbhtid
         r <- eitherSnapForm (entryForm htid) "add-entry-form"
         case r of
             Left splices' -> do
               {-liftIO $ putStrLn "Left"
               liftIO $ putStrLn $ show $ map fst splices'-}
               heistLocal (bindSplices splices') $ renderHT "entries/add"
             {-Left form' -> 
               heistLocal (bindSplices [("formdata", (formSplice form')),("target-div", return [X.TextNode "#add-form"])]) $ renderHT "form"-}
             Right entry' -> do
               {-liftIO $ putStrLn "Right"-}
               saveHouseTabEntry entry'
               recalculateTotals user
               {-setNotification "Successfully added entry."-}
               renderHT "entries/add_success"                
 
{-editEntry :: User -> Application ()
editEntry user = 
  do i <- getParam "id"
     case i of
      Just uid' -> do
        let uid = read $ B8.unpack uid'
        r <- eitherSnapForm (entryForm (find ((== uid).eid) (houseTabEntries user))) "edit-entry-form" 
        case r of
          Left form' -> 
            heistLocal (bindSplice "formdata" (formSplice form')) $ renderHT "form"
          Right entry' -> do
            modEntries (U.findReplace ((== uid).eid) entry') user
            redirect "/entries"
      Nothing -> redirect "/entries"

deleteEntry :: User -> Application ()
deleteEntry user = 
  do i <- getParam "id"
     case i of
      Just uid' -> do
        let uid = read $ B8.unpack uid'
        r <- eitherSnapForm deleteForm "delete-entry-form" 
        case r of
          Left form' -> 
            heistLocal (bindSplice "formdata" (formSplice form')) $ renderHT "form"
          Right _ -> do
            modEntries (filter ((/= uid).eid)) user
            redirect "/entries"
      Nothing -> redirect "/entries"-}

validDate :: Validator Application Text Date
validDate = check "Must be a valid date, like 2011.2.25" $ \(Date y m d) -> and [y>1900,y<2100,m>=1,m<=12,d>=1,d<=31]

positive :: (Ord a, Num a) => Validator Application Text a
positive = check "Must be a positive number." $ \n -> n > 0

entryForm :: BS.ByteString -> SnapForm Application Text HeistView HouseTabEntry
entryForm htid = mkEntry
    <$> input "id"      Nothing
    <*> input "by"      Nothing  `validate` onePerson <++ errors
    <*> input "for"     Nothing  `validate` manyPeople    <++ errors 
    <*> inputRead "ammount" "Invalid ammount" Nothing  <++ errors 
    <*> input "what" Nothing   <++ errors 
    <*> inputRead "date" "invalid Date" Nothing     <++ errors 
  where mkEntry i b f a wha whe = HouseTabEntry (strMaybe i) htid (B8.pack b) (B8.pack wha) whe a (B8.pack f)
        strMaybe s = case s of
                      "" -> Nothing
                      x -> Just (B8.pack x)


{-deleteForm :: SnapForm Application Text HeistView ()
deleteForm = const ()
    <$> input "id" Nothing-}

