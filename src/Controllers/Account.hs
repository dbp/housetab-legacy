{-# LANGUAGE OverloadedStrings #-}

module Controllers.Account where

import            Snap.Auth
import            Snap.Auth.Handlers
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
import            Mail (mailActivation)
import            Views.Site
import            Controllers.Form

import            Models.Account



{-currentHouseTab :: Application (Maybe ([Person],[HouseTabEntry]))
currentHouseTab = do entries <- currentEntries
                     people <- currentPeople
                     return $ liftM2 (,) people entries-}


requireUserBounce :: Application () -> Application ()
requireUserBounce good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    requireUser loginPage good

requireUserBounce' :: (User -> Application ()) -> Application ()
requireUserBounce' good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    u <- currentUser
    case u of
      Nothing -> loginPage
      Just user -> good user
 

redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r

-- Make sure you have a 'password' field in there
newSessionH :: a -> Application ()
newSessionH = \_ -> renderHT "login"

-- Assuming you have a signup.tpl template
newSignupH = renderHT "signup"

unDoc (Doc fs) = fs

-- Save user and redirect as appropriate
signupH :: Application ()
signupH = do
  ps <- getParams
  token <- liftIO $ getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  let user = makeUser token ps
  au <- maybe (return Nothing) (\u -> saveAuthUser (authUser u, additionalUserFields u)) user 
  case au of
    Nothing -> newSignupH
    Just au' -> do --setSessionUserId $ userId au'
                   let user' = fromJust user -- we know this won't fail. for au to be Just, so must user
                   mailActivation token (accountName user') (accountEmails user')
                   redirect "/"
                   
activateAccountH :: Application ()
activateAccountH = do
  token         <- getParam "token"
  maccountName  <- getParam "account"  -- will this work?: liftM fromJust $ getParam "account"
  -- this is not the best user experience - better to present a message saying the token is missing, probably
  guard $ all isJust [token,maccountName]
  let accountName = fromJust maccountName
  
  res <- withDB $ modify (select ["accountName" =: accountName, "accountActivate" =: token] "users") ["$set" =: ["accountActivate" =: (Nothing :: Maybe BS.ByteString)]]
  either (const $ redirect "/") (const $ redirect "/login") res 
