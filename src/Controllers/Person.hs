{-# LANGUAGE OverloadedStrings #-}

module Controllers.Person where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB hiding (group, sort, Array)
import qualified  Data.Map as M
import            Control.Monad
import            Control.Applicative
import            Control.Monad.Trans
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T

import            Data.Aeson
import qualified  Data.Vector as V

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust)
import qualified  Data.Bson as B
import            Data.List (sortBy, sort, group)
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
{-import            Views.Person-}
import            Models.Person
import            Models.Account
import            Controllers.Form

personCheck :: Validator Application Text Person
personCheck = check "Shouldnt see this" $ \(Person _ _ _ _) -> True


onePerson :: Validator Application Text String
onePerson = checkM "Must be a id that corresponds to a person." fn
  where fn id' = do pers <- getHouseTabPerson (B8.pack id')
                    return $ isJust pers

manyPeople :: Validator Application Text String
manyPeople = checkM "Must be comma separated ids that corresponds to people, no duplicates." fn
  where fn ps = do muid <- authenticatedUserId
                   case muid of
                     Nothing -> return False -- this should never be able to happen, but...
                     Just uid -> do
                       peop <- getHouseTabPeople (emptyAuthUser {userId = Just uid})
                       case peop of
                         [] -> return False -- couldnt get their people. this means db error, but nothing we can do here
                         people -> return $ and (map ((flip elem) (map (B8.unpack . maybe "" objid2bs . pId) people)) (splitOn "," ps)) && noDuplicates (splitOn "," ps)
        noDuplicates = (all (== 1)) . (map length) . group . sort


                     
addPerson :: User -> Application ()
addPerson user = do
   r <- eitherSnapForm (personForm Nothing) "add-person-form"
   case r of
       Left splices' -> 
         heistLocal (bindSplices splices') $ renderHT "people/add"
       Right person' -> do
         mhtid <-  authenticatedUserId
         case mhtid >>= (\(UserId h) -> bs2objid h) of
           Nothing -> renderHT "people/add_failure"  
           Just htid -> do saveHouseTabPerson $ person' { pHTId = htid}
                           renderHT "people/add_success"  


addShare :: User -> Application ()
addShare user = do
   mPid <- getParam "person"
   case mPid of
     Nothing -> renderHT "people/share/no_person"
     Just pid -> do
       r <- eitherSnapForm (shareForm Nothing) "add-share-form"
       case r of
           Left splices' -> 
             heistLocal (bindSplices splices') $ renderHT "people/share/add"
           Right share' -> do
             mhtid <- authenticatedUserId
             mperson <- getHouseTabPerson pid
             case mhtid of
               Nothing -> renderHT "people/share/add_failure" -- should never happen
               Just htid -> 
                  case mperson of
                    Nothing -> renderHT "people/share/add_failure" -- means they hit the wrong URL
                    Just person -> do saveHouseTabPerson $ person { pShares = share':(pShares person)}
                                      renderHT "people/share/add_success"  
                                      

listPeople = do mhtid <- authenticatedUserId
                case mhtid of
                  Nothing -> mzero
                  Just uid -> do people <- getHouseTabPeople (emptyAuthUser {userId = Just uid})
                                 {-liftIO $ putStrLn $ show people-}
                                 getResponse >>= return . setContentType "text/json" >>= putResponse
                                 writeLBS $ encode $ Array $ V.fromList $ map renderPerson (pwithIds people)
    where renderPerson p = object ["id" .= objid2bs (fromJust $ pId p), "name" .= TE.decodeUtf8 (pName p)]
          pwithIds = filter (isJust . pId)

{-editPerson :: User -> Application ()
editPerson user = 
  do l' <- getParam "letter"
     case (l',liftM BS.length l') of
      (Just letr, Just 1) -> do
        let l = head $ B8.unpack letr 
        r <- eitherSnapForm (personForm (find ((== l).letter) (houseTabPeople user))) "edit-person-form" 
        case r of
          Left form' -> 
            heistLocal (bindSplice "formdata" (formSplice form')) $ renderHT "form"
          Right person' -> do
            modPeople (U.findReplace ((== l).letter) person') user
            redirect "/entries"
      _ -> redirect "/entries"
      -}

nonNegative :: (Ord a, Num a) => Validator Application Text a
nonNegative = check "Must be a non-negative number (can be decimal) like 0.2." $ \n -> n >= 0

shareForm :: Maybe Share -> SnapForm Application Text HeistView Share
shareForm p = Share
    <$> inputRead "date" "Must be a date, like 2011.6.30" (liftM sDate p)   `validate` validDate <++ errors
    <*> inputRead "share" "Must be a number"               (liftM sValue p)  `validate` nonNegative   <++ errors
        

personForm :: Maybe Person -> SnapForm Application Text HeistView Person
personForm p = mkPerson
    <$> input "id"   (fmap (B8.unpack . objid2bs) $ pId =<< p)
    <*> input "name" (lM pName p)   `validate` nonEmpty <++ errors
   where mkPerson i n = Person (bs2objid $ B8.pack i) emptyObjId (B8.pack n) (fromMaybe [] $ liftM pShares p)
         lM f = liftM (B8.unpack . f)
         emptyObjId = Oid 0 0
        