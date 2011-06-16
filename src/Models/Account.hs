{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Account where

import            Snap.Extension
import qualified  Snap.Extension.DB.MongoDB as DB
import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import qualified  Data.Map as M

import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Typeable
import            Data.Maybe (catMaybes, listToMaybe)
import            Data.List.Split (splitOn)
import            Data.List (sortBy)
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader
import            Application
import            Lib

import            Models.Result
import            Models.Person
import            Models.Entry

data User = User
  { authUser        :: AuthUser
  , accountName     :: BS.ByteString
  , accountEmails   :: [BS.ByteString]
  {-, houseTabEntries :: [HouseTabEntry]-}
  , houseTabPeople  :: [Person]
  , currentResult    :: Result
  , accountReset    :: Maybe BS.ByteString -- reset token
  , accountActivate :: Maybe BS.ByteString -- activation token
  }

-- Construct your 'User' from the given parameters
-- Make sure you do validation as well - at least for now.
makeUser token ps = do
  password  <- look "password"  ps
  name      <- look "name"      ps
  emails    <- look "email"    ps
  return (User emptyAuthUser { userPassword = Just (ClearText password) } name [emails] [] emptyResult Nothing (Just token))
        where look key map = liftM (BS.intercalate " ") $ M.lookup key map

additionalUserFields :: User -> Document
additionalUserFields u = [ "accountName"      =: accountName u
                         , "accountEmails"    =: accountEmails u
                         {-, "houseTabEntries"  =: houseTabEntries u-}
                         , "houseTabPeople"   =: houseTabPeople u
                         , "currentResult"    =: currentResult u
                         , "accountReset"     =: accountReset u -- reset token
                         , "accountActivate"  =: accountActivate u -- activation token
                         ]


currentUser :: Application (Maybe User)
currentUser = do  u <- currentAuthUser 
                  -- we set the accountName in the session at this point as well
                  let resp = do fields    <- liftM snd u
                                auth      <- liftM fst u
                                name      <- B.lookup "accountName"       fields
                                emails    <- B.lookup "accountEmails"     fields
                                {-entries   <- B.lookup "houseTabEntries"   fields-}
                                people    <- B.lookup "houseTabPeople"    fields
                                current   <- B.lookup "currentResult"     fields
                                reset     <- B.lookup "accountReset"      fields      
                                activate  <- B.lookup "accountActivate"   fields      
                                return $ User auth name emails {-entries-} people current reset activate
                  maybe (return ()) (setInSession "accountName") (liftM accountName resp)
                  return resp 

currentPeople :: Application (Maybe [Person])
currentPeople = liftM (liftM houseTabPeople) currentUser


modPeople :: ([Person] -> [Person]) -> User -> Application ()
modPeople fn user = do
   let u' = user {houseTabPeople = sortPeople (fn (houseTabPeople user))}
   recalculateTotals u'
 where sortPeople = sortBy (\p1 p2 -> compare (letter p1) (letter p2))  
 
 
recalculateTotals :: User -> Application ()
recalculateTotals u = do 
  entries <- getHouseTabEntries (authUser u)
  let u' = u {currentResult = run (houseTabPeople u) entries}
  saveAuthUser (authUser u', additionalUserFields u')
  return ()

