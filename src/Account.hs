{-# LANGUAGE OverloadedStrings #-}

module Account where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB
import qualified  Data.Map as M
import            Control.Monad
import            Control.Monad.Trans
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust)
import qualified  Data.Bson as B
import            Data.List (sortBy)
import            System.Random

import            Application
import            State
import            Lib

data User = User
  { authUser        :: AuthUser
  , accountName     :: BS.ByteString
  , accountEmails   :: [BS.ByteString]
  , houseTabEntries :: [HouseTabEntry]
  , houseTabPeople  :: [Person]
  , currentResult    :: Result
  , accountReset    :: Maybe BS.ByteString -- reset token
  , accountActivate :: Maybe BS.ByteString -- activation token
  }

currentUser :: Application (Maybe User)
currentUser = do  u <- currentAuthUser 
                  -- we set the accountName in the session at this point as well
                  let resp = do fields    <- liftM snd u
                                auth      <- liftM fst u
                                name      <- B.lookup "accountName"       fields
                                emails    <- B.lookup "accountEmails"     fields
                                entries   <- B.lookup "houseTabEntries"   fields
                                people    <- B.lookup "houseTabPeople"    fields
                                current   <- B.lookup "currentResult"     fields
                                reset     <- B.lookup "accountReset"      fields      
                                activate  <- B.lookup "accountActivate"   fields      
                                return $ User auth name emails entries people current reset activate
                  maybe (return ()) (setInSession "accountName") (liftM accountName resp)
                  return resp 

currentEntries :: Application (Maybe [HouseTabEntry])
currentEntries = liftM (liftM houseTabEntries) currentUser

currentPeople :: Application (Maybe [Person])
currentPeople = liftM (liftM houseTabPeople) currentUser

currentHouseTab :: Application (Maybe ([Person],[HouseTabEntry]))
currentHouseTab = do entries <- currentEntries
                     people <- currentPeople
                     return $ liftM2 (,) people entries

modEntries :: ([HouseTabEntry] -> [HouseTabEntry]) -> User -> Application ()
modEntries fn user = do
   let u' = user {houseTabEntries = sortEntries (fn (houseTabEntries user))}
   let u'' = u' {currentResult = run (houseTabPeople u') (houseTabEntries u')}
   saveAuthUser (authUser u'', additionalUserFields u'')
   return ()
 where sortEntries = sortBy (\e1 e2 -> compare (ewhen e1) (ewhen e2))  

modPeople :: ([Person] -> [Person]) -> User -> Application ()
modPeople fn user = do
   let u' = user {houseTabPeople = sortPeople (fn (houseTabPeople user))}
   let u'' = u' {currentResult = run (houseTabPeople u') (houseTabEntries u')}
   saveAuthUser (authUser u'', additionalUserFields u'')
   return ()
 where sortPeople = sortBy (\p1 p2 -> compare (letter p1) (letter p2))  
 
-- Construct your 'User' from the given parameters
-- Make sure you do validation as well - at least for now.
makeUser token ps = do
  password  <- look "password"  ps
  name      <- look "name"      ps
  emails    <- look "email"    ps
  return (User emptyAuthUser { userPassword = Just (ClearText password) } name [emails] [] [] emptyResult Nothing (Just token))
        where look key map = liftM (BS.intercalate " ") $ M.lookup key map

additionalUserFields :: User -> Document
additionalUserFields u = [ "accountName"      =: accountName u
                         , "accountEmails"    =: accountEmails u
                         , "houseTabEntries"  =: houseTabEntries u
                         , "houseTabPeople"   =: houseTabPeople u
                         , "currentResult"    =: currentResult u
                         , "accountReset"     =: accountReset u -- reset token
                         , "accountActivate"  =: accountActivate u -- activation token
                         ]

redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             ps <- getParams
             redirect $ fromMaybe "/" r

-- Make sure you have a 'password' field in there
newSessionH :: a -> Application ()
newSessionH = \_ -> render "login"

-- Assuming you have a signup.tpl template
newSignupH = render "signup"

unDoc (Doc fs) = fs

-- Save user and redirect as appropriate
signupH :: Application ()
signupH = do
  ps <- getParams
  token <- liftIO $ getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  au <- maybe (return Nothing) (\u -> saveAuthUser (authUser u, additionalUserFields u)) (makeUser token ps) 
  case au of
    Nothing -> newSignupH
    Just au' -> do setSessionUserId $ userId au'
                   redirect "/"
                   
activateAccountH :: Application ()
activateAccountH = do
  token         <- getParam "token"
  maccountName  <- getParam "account"  -- will this work?: liftM fromJust $ getParam "account"
  -- this is not the best user experience - better to present a message saying the token is missing, probably
  guard $ all isJust [token,maccountName]
  let accountName = fromJust maccountName
  
  res <- withDB $ modify (select ["accountName" =: accountName, "accountActivate" =: token] "users") ["$set" =: ["accountActivate" =: (Nothing :: Maybe BS.ByteString)]]
  liftIO $ putStrLn $ show res
  either (const $ redirect "/") (const $ redirect "/login") res 
