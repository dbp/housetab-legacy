{-# LANGUAGE OverloadedStrings #-}

module Controllers.Account where

import            Snap.Auth
import            Snap.Auth.Handlers
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB
import qualified  Data.Map as M
import            Control.Monad
import            Control.Monad.Reader
import            Control.Monad.Trans
import            Control.Applicative

import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as TE

import            Snap.Extension.Heist
import            Snap.Extension.Heist.Impl
import            Data.Maybe (fromMaybe, fromJust, isJust)
import qualified  Data.Bson as B
import            Data.List (sortBy)
import            System.Random

import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform
import            Text.Templating.Heist

import            Data.Time.LocalTime
import            Data.Time.Format
import            System.Locale (defaultTimeLocale)


import            Application
import            State
import            Lib
import            Mail (mailActivation)
import            Views.Site
import            Views.Result
import            Views.Account
import            Views.Person
import            Controllers.Form

import            Models.Account
import            Models.Site


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
      Just user -> do hs <- liftM getHeistState ask
                      peopleSplice <- getPeopleSplices (authUser user)
                      now <- liftIO $ getLocalTime
                      let today = localDay now
                      registerSplices hs 
                        ([ ("tutorial", tutorialSplice)
                         , ("historyOn", if (recordHistory user) then identitySplice else blackHoleSplice)
                         , ("historyOff", if (recordHistory user) then blackHoleSplice else identitySplice)
                         , ("currentDateLong", textSplice $ T.pack $ formatTime defaultTimeLocale "%e %B %Y" today)
                         -- the following should be the default until overridden by digestive-functors validation
                         , ("date-value",textSplice $ T.pack $ formatTime defaultTimeLocale "%-m.%d.%Y" today)
                         , ("accountName",      textSplice $ TE.decodeUtf8 (accountName user))
                         ] ++ peopleSplice)
                      good user
 
noRequireUser :: Application () -> Application ()
noRequireUser handler = do 
  hs <- liftM getHeistState ask
  registerSplices hs [("tutorial", blackHoleSplice)]
  handler

redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r

-- Make sure you have a 'password' field in there
loginH :: Application ()
loginH = renderHT "account/login"

loginSuccess :: Application ()
loginSuccess = do u <- currentUser
                  recalculateTotals (fromJust u)
                  redirTo

unDoc (Doc fs) = fs

-- Save user and redirect as appropriate
signupH :: Application ()
signupH = do
  r <- eitherSnapForm signupForm "signup-form"
  case r of
    Left splices' -> 
      heistLocal (bindSplices splices') $ renderHT "account/signup"
    Right creds -> do  
      token <- liftIO $ getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
      let user = makeUser token creds
      au <- saveAuthUser (authUser user, additionalUserFields user) 
      case au of
        Nothing -> redirect "/signup" -- something went wrong in db... oops
        Just au' -> do --setSessionUserId $ userId au'
                       mailActivation token (accountName user) (accountEmails user)
                       renderHT "account/signup_success"
                   
signupForm :: SnapForm Application T.Text HeistView SignupCreds
signupForm = mkSignupCreds
    <$> input "name" Nothing `validate` nonEmpty <++ errors
    <*> passwordForm
    <*> input "email" Nothing `validate` validEmail <++ errors
   where mkSignupCreds n (NewPassword p _) e  = SignupCreds (B8.pack n) (B8.pack p) (B8.pack e)


activateAccountH :: Application ()
activateAccountH = do
  token         <- getParam "token"
  maccountName  <- getParam "account"  -- will this work?: liftM fromJust $ getParam "account"
  -- this is not the best user experience - better to present a message saying the token is missing, probably
  guard $ all isJust [token,maccountName]
  let accountName = fromJust maccountName
  res <- withDB $ modify (select ["accountName" =: accountName, "accountActivate" =: token] "users") ["$set" =: ["accountActivate" =: (Nothing :: Maybe BS.ByteString)]]
  either (const $ redirect "/") (const $ redirect "/login") res 

passwordForm :: SnapForm Application T.Text HeistView NewPassword
passwordForm = (`validate` matchingPasswords) $ (<++ errors) $ NewPassword
    <$> input "password" Nothing  `validate` nonEmpty <++ errors 
    <*> input "confirm"  Nothing  `validate` nonEmpty <++ errors 
  where matchingPasswords = check "Passwords do not match." $ \(NewPassword p1 p2) -> p1 == p2
        mkPW p1 p2 = NewPassword p1 p2

resetPasswordH :: Application ()
resetPasswordH = do
  token         <- getParam "token"
  maccountName  <- getParam "account"  -- will this work?: liftM fromJust $ getParam "account"
  -- this is not the best user experience - better to present a message saying the token is missing, probably
  guard $ all isJust [token,maccountName]
  let accountName = fromJust maccountName
  r <- eitherSnapForm passwordForm "password-reset-form"
  case r of
    Left splices' -> 
      heistLocal (bindSplices splices') $ renderHT "account/reset"
    Right (NewPassword pw _) -> do  
      muser <- withDB $ findOne (select ["accountName" =: accountName, "accountReset" =: token] "users")
      case muser of
        Left _ -> redirect "/"
        Right user' -> 
          case user' of
            Nothing -> redirect "/"
            Just user -> do
              let n = fmap dropReset $ buildUser $ 
                        (,) <$> (fmap (setPass (B8.pack pw)) (docToAuthUser user)) 
                            <*> (Just user)
              maybe (redirect "/") (\u -> do saveAuthUser (authUser u, additionalUserFields u)
                                             redirect "/login") n
              
 where dropReset u = u { accountReset = Nothing }
       setPass p au = au {userPassword = Just $ ClearText p}

       
tutorialDeactivate :: User -> Application ()
tutorialDeactivate user = do let u = user { tutorialActive = False }
                             saveAuthUser (authUser u, additionalUserFields u)
                             deleteFromSession "tutorial-step"
                             heistLocal (bindSplices [ ("result", (renderResult  $ currentResult user))
                                                     ])
                              $ renderHT "tutorial/deactivated"

tutorialActivate :: User -> Application ()
tutorialActivate user = do let u = user { tutorialActive = True }
                           saveAuthUser (authUser u, additionalUserFields u)
                           setInSession "tutorial-step" "1"
                           renderHT "tutorial/activated"
                           