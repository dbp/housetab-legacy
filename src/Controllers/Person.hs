{-# LANGUAGE OverloadedStrings #-}

module Controllers.Person where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB hiding (group, sort)
import qualified  Data.Map as M
import            Control.Monad
import            Control.Monad.Trans
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust)
import qualified  Data.Bson as B
import            Data.List (sortBy, sort, group)
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
{-import            Views.Person-}
import            Models.Person
import            Models.Account
import            Controllers.Form

personCheck :: Validator Application Text Person
personCheck = check "Shouldnt see this" $ \(Person n l ps) -> True


onePerson :: Validator Application Text String
onePerson = checkM "Must be a single letter that corresponds to a person." fn
  where fn p = do peop <- currentPeople
                  case peop of
                    Nothing -> return False
                    Just people -> return $ length p == 1 && (head p) `elem` (map letter people)

manyPeople :: Validator Application Text String
manyPeople = checkM "Must be all letters that corresponds to people, no duplicates." fn
  where fn p = do peop <- currentPeople
                  case peop of
                    Nothing -> return False
                    Just people -> return $ and (map ((flip elem) (map letter people)) p) && noDuplicates p
        noDuplicates = (all (== 1)) . (map length) . group . sort


  {-                   
addPerson :: User -> Application ()
addPerson user = do
   r <- eitherSnapForm (personForm Nothing) "add-person-form"
   case r of
       Left form' -> 
         heistLocal (bindSplice "formdata" (formSplice form')) $ renderHT "form"
       Right person' -> do
         modPeople ([person'] ++) user
         redirect "/entries"

editPerson :: User -> Application ()
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
      
zeroOne :: (Ord a, Num a) => Validator Application Html a
zeroOne = check "Must be a number benween zero and one, like 0.2." $ \n -> n >= 0 && n <= 1

percentForm :: Maybe Percent -> SnapForm Application Html BlazeFormHtml Percent
percentForm p = mkPercent
    <$> label "Date: "  ++> inputTextRead "Must be a date, like 2011.6.30" (liftM pDate p)   `validate` validDate <++ errors
    <*> label "Value: " ++> inputTextRead "Must be a number"               (liftM pValue p)  `validate` zeroOne   <++ errors
  where mkPercent d v = Percent d v
        

personForm :: Maybe Person -> SnapForm Application Html BlazeFormHtml Person
personForm p = mkPerson
    <$> label "Name: "      ++> inputText (lM name p)   `validate` nonEmpty <++ errors
    <*> label "Letter: "    ++> inputText (liftM ((:[]).letter) p) `validate` lenOne   <++ errors
    <*> label "Percents: "  ++> percentInput (liftM percs p)                <++ errors    
  where mkPerson n l ps = Person (B8.pack n) (head l) ps
        lM f = liftM (B8.unpack . f)
        percentInput ps = inputList hiddenInt
                                    (percentForm)
                                    ps
        hiddenInt = transformFormlet show inputHidden $ transformRead "Internal error"-}
        