{-# LANGUAGE OverloadedStrings #-}

module Auth where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB
import qualified  Data.Map as M
import            Control.Monad
import            Snap.Types
import qualified  Data.ByteString as BS
import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust)

import            Application
import            State

data User = User
  { authUser :: AuthUser,
    accountName :: BS.ByteString,
    accountId :: BS.ByteString
  }


-- Construct your 'User' from the given parameters
-- Make sure you do validation as well - at least for now.
makeUser ps name accnt = User emptyAuthUser { userPassword = liftM ClearText (look "password" ps)}
                         name
                         accnt
        where look key map = liftM (BS.intercalate " ") $ M.lookup key map

additionalUserFields :: User -> Document
additionalUserFields u = [ "accountName" =: accountName u
                         , "accountId"   =: accountId u]

redirHome :: Application ()
redirHome = redirect "/"

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
  n <- getParam "name"
  case n of
    Nothing -> newSignupH
    Just name -> do
      res <- withDB $ insert "accounts" $ unDoc $ val $ emptyAccount {aname = name}
      case res of
        Left _ -> newSignupH
        Right id -> do
           let u = makeUser ps name (fromJust $ cast' id)
           au <- saveAuthUser (authUser u, additionalUserFields u)
           case au of
             Nothing -> newSignupH
             Just au' -> do setSessionUserId $ userId au'
                            redirect "/"
          
