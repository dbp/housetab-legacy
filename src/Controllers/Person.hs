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
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
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

import            Data.Time.Calendar
import            Data.Time.LocalTime
import            Data.Time.Clock

import            Application
import            State
import            Lib
import            Mail (mailActivation)
import            Utils
import            Views.Site
import            Views.Result
import            Views.Person
import            Models.Person
import            Models.Site
import            Models.Account
import            Models.Entry
import            Controllers.Form

personCheck :: Validator Application Text Person
personCheck = check "Shouldnt see this" $ \(Person _ _ _ _) -> True


onePerson :: Validator Application Text String
onePerson = checkM "Must be a id that corresponds to a person." fn
  where fn id' = do pers <- maybe (return Nothing) getHouseTabPerson (bs2objid $ B8.pack id')
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
                           nu <- recalculateTotals user
                           when (tutorialActive user) $ setInSession "tutorial-step" "2"
                           (heistLocal $ bindSplices [("result",(renderResult  $ currentResult nu))]) $ renderHT "people/add_success"  

personH :: User -> Application ()
personH user = do
  mPid <- getParam "person"
  case mPid of
    Nothing -> redirect "/settings"
    Just pid -> do
      mperson <- maybe (return Nothing) getHouseTabPerson (bs2objid pid)
      case mperson of
        Nothing -> redirect "/settings" -- means they hit the wrong URL
        Just person -> route [("/shares",               showShares user person)
                             ,("/shares/hide",          hideShares user person)
                             ,("/shares/add",           addShare user person)
                             ,("/shares/delete/:date",  deleteShare user person)
                             ]

hideShares :: User -> Person -> Application ()
hideShares user person = heistLocal (bindSplices (renderPerson person)) $ renderHT "people/share/hide"

shareErrorsBlank = [("date-errors", blackHoleSplice)
                   ,("value-errors", blackHoleSplice)
                   ,("value-value", textSplice "")
                   ]

showShares :: User -> Person -> Application ()
showShares user person = heistLocal (bindSplices (shareErrorsBlank ++ (renderPerson person))) $ renderHT "people/share/show"


addShare :: User -> Person -> Application ()
addShare user person = do
   r <- eitherSnapForm (shareForm Nothing) "add-share-form"
   case r of
       Left splices' -> 
         heistLocal 
          (bindSplices (splices' ++ (renderPerson person))) $ 
          renderHT "people/share/add"
       Right share' -> do
         mhtid <- authenticatedUserId
         case mhtid of
           Nothing -> redirect "/settings" -- should never happen
           Just htid -> 
              do let nperson = person { pShares = share':(pShares person)}
                 saveHouseTabPerson nperson
                 nu <- recalculateTotals user
                 people <- getHouseTabPeople (authUser user)
                 today <- liftM localDay $ liftIO getLocalTime
                 heistLocal (bindSplices 
                    [ ("result",      (renderResult  $ currentResult nu))
                    , ("totalShares", textSplice $ T.pack $ show $ getTotalShares today people)])
                  $ renderHT "people/share/change_success"
                 {-heistLocal (bindSplices (renderPerson nperson)) $ renderHT "people/share/show"-}

deleteShare :: User -> Person -> Application ()
deleteShare user person = do
  d <- getParam "date"
  case d >>= (maybeRead . B8.unpack) of
    Just date -> do
      let nperson = person { pShares = filter ((/= date).sDate) (pShares person) }
      saveHouseTabPerson nperson
      nu <- recalculateTotals user
      people <- getHouseTabPeople (authUser user)
      today <- liftM localDay $ liftIO getLocalTime
      heistLocal (bindSplices 
         [ ("result",      (renderResult  $ currentResult nu))
         , ("totalShares", textSplice $ T.pack $ show $ getTotalShares today people)])
       $ renderHT "people/share/change_success"
      {-heistLocal (bindSplices ((renderPerson nperson) ++ shareErrorsBlank)) $ renderHT "people/share/show"-}
    Nothing -> redirect "/settings"
                                       

listPeople = do mhtid <- authenticatedUserId
                case mhtid of
                  Nothing -> mzero
                  Just uid -> do people <- getHouseTabPeople (emptyAuthUser {userId = Just uid})
                                 {-liftIO $ putStrLn $ show people-}
                                 getResponse >>= return . setContentType "text/json" >>= putResponse
                                 writeLBS $ encode $ Array $ V.fromList $ map renderPerson (pwithIds people)
    where renderPerson p = object ["id" .= objid2bs (fromJust $ pId p), "name" .= TE.decodeUtf8 (pName p)]
          pwithIds = filter (isJust . pId)

editPerson :: User -> Application ()
editPerson user = 
  do i <- getParam "id"
     let mbhtid = userId $ authUser user
     when (isNothing mbhtid) $ redirect "/settings"
     let (UserId htid) = fromJust mbhtid
     case i >>= bs2objid of
      Just pid -> do
        mperson <- getHouseTabPerson pid
        case mperson of 
          Nothing -> redirect "/settings"
          Just person -> do
            r <- eitherSnapForm (personForm (Just person)) "edit-person-form" 
            case r of
              Left splices' -> 
                heistLocal (bindSplices splices') $ renderHT "people/edit"
              Right person' -> do
                let nperson = person { pName = pName person' }
                saveHouseTabPerson nperson
                nu <- recalculateTotals user
                heistLocal (bindSplices  (renderPerson nperson)) $ renderHT "people/name"
      Nothing -> redirect "/settings"
      

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
   where mkPerson i n = Person (bs2objid $ B8.pack i) emptyObjectId (B8.pack n) (fromMaybe [] $ liftM pShares p)
         lM f = liftM (B8.unpack . f)
        