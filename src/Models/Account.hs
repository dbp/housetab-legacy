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
import            Models.Site

data User = User
  { authUser           :: AuthUser
  , accountName        :: BS.ByteString
  , accountEmail       :: BS.ByteString
  , accountEmailChange :: Maybe (BS.ByteString, BS.ByteString) -- token, new email
  , currentResult      :: Result
  , accountReset       :: Maybe BS.ByteString -- reset token
  , accountActivate    :: Maybe BS.ByteString -- activation token
  , tutorialActive     :: Bool
  , recordHistory      :: Bool
  }

data SignupCreds = SignupCreds { suName :: BS.ByteString
                               , suPassword :: BS.ByteString
                               , suEmail :: BS.ByteString
                               }

data NewPassword = NewPassword String String

data NewSettings = NewSettings BS.ByteString BS.ByteString
                     
getTotalSpent :: User -> Double
getTotalSpent u = sum $ map (\(_,s,_) -> s) $ people (currentResult u)


makeUser token (SignupCreds name password email) =
  (User emptyAuthUser { userPassword = Just (ClearText password) } name email Nothing emptyResult Nothing (Just token) True True)

additionalUserFields :: User -> Document
additionalUserFields u = [ "accountName"        =: accountName u
                         , "accountEmail"       =: accountEmail u
                         , "accountEmailChange" =: (liftM untuple $ accountEmailChange u)
                         , "currentResult"      =: currentResult u
                         , "accountReset"       =: accountReset u -- reset token
                         , "accountActivate"    =: accountActivate u -- activation token
                         , "tutorialActive"     =: tutorialActive u
                         , "recordHistory"      =: recordHistory u
                         ]


currentUser :: Application (Maybe User)
currentUser = do  u <- currentAuthUser 
                  -- we set the accountName in the session at this point as well
                  let resp = buildUser u
                  maybe (return ()) (setInSession "accountName") (liftM accountName resp)
                  return resp 
                  
buildUser :: (Maybe (AuthUser, Document)) -> Maybe User
buildUser u = do fields       <- liftM snd u
                 auth         <- liftM fst u
                 name         <- B.lookup "accountName"         fields
                 email        <- B.lookup "accountEmail"        fields
                 emailchange  <- B.lookup "accountEmailChange"  fields
                 current      <- B.lookup "currentResult"       fields
                 reset        <- B.lookup "accountReset"        fields      
                 activate     <- B.lookup "accountActivate"     fields      
                 tutorial     <- B.lookup "tutorialActive"      fields      
                 history      <- B.lookup "recordHistory"       fields      
                 return $ User auth name email (tuple =<< emailchange) current reset activate tutorial history
 
recalculateTotals :: User -> Application User
recalculateTotals u = do 
  entries <- getHouseTabEntriesAll (authUser u)
  people <- getHouseTabPeople (authUser u)
  let u' = u {currentResult = run people entries}
  saveAuthUser (authUser u', additionalUserFields u')
  return u'


deleteAccount :: User -> Application ()
deleteAccount user =
  case userId (authUser user) >>= (DB.bs2objid . unUid) of
    Nothing -> return () -- no id means we can't do anything, but this should never happen
    Just htid -> do
      DB.withDB $ DB.delete $ DB.select ["htid" =: htid] "entries"
      DB.withDB $ DB.delete $ DB.select ["htid" =: htid] "history"
      DB.withDB $ DB.delete $ DB.select ["htid" =: htid] "people"
      DB.withDB $ DB.delete $ DB.select ["_id" =: htid]  "users"
      -- fewww... that was a lot of work.
      return ()
