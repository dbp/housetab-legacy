{-# LANGUAGE OverloadedStrings #-}
module Controllers.Form where
  
import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform

import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Data.Bson
import            Data.List.Split
import qualified  Data.Text as T

import            Snap.Extension.DB.MongoDB

import qualified  Data.Map as M

import            Data.Maybe (isNothing)

import            Snap.Auth

import            Data.Text (Text)
import            Models.Entry
import            Control.Applicative
import            Control.Monad
import            Text.Templating.Heist

import            Snap.Auth

import            Application

import            Models.Account
import            Models.Site

import            Views.Site



nonEmpty :: Validator Application Text String
nonEmpty = check "Must not be empty." $ \s -> not $ null s
    
lenOne :: Validator Application Text String
lenOne = check "Must be a single character." $ \s -> length s == 1


validDate :: Validator Application Text Date
validDate = check "Must be a valid date, like 2.25.2011" $ \(Date y m d) -> and [y>1900,y<2100,m>=1,m<=12,d>=1,d<=31]

positive :: (Ord a, Num a) => Validator Application Text a
positive = check "Must be a positive number." $ \n -> n > 0

isCategory :: Validator Application Text String
isCategory = check "You must select a category." $ \c -> (T.pack c) `elem` categoryList

mongoObjectId :: Transformer Application Text String ObjectId
mongoObjectId = transformEither (\a -> maybe (Left "Invalid Object Id Specified") Right (bs2objid (B8.pack a)))

mongoObjectIdMany :: Transformer Application Text String [ObjectId]
mongoObjectIdMany = transformEither (\a -> maybe (Left "Invalid Object Id List Specified") Right (sequence $ map (bs2objid . B8.pack) (splitOn "," a)))

-- | regex validation sucks, so don't even try.
validEmail :: Validator Application Text String
validEmail = check "Must be a valid email, like info@housetab.org" $ \e -> '@' `elem` e && '.' `elem` e

checkPassword :: User -> Validator Application Text String
checkPassword user = checkM "Incorrect password." $ \password ->
  do res <- performLogin (EUId (M.fromList [("accountName", [accountName user])])) (B8.pack password) False
     case res of
       Left _ -> return False -- not the right credentials
       Right _ -> return True

noSpaces :: Validator Application Text String
noSpaces = check "Cannot have spaces" $ \word -> not $ ' ' `elem` word
       
uniqueUser :: Validator Application Text String
uniqueUser = checkM "Username already taken, please choose another" $ \user ->
  do ex <- withDB $ findOne $ select ["accountName" =: user] "users"
     case ex of
       Left _ -> return False -- something went wrong, play it on the safe side
       Right r -> return $ isNothing r