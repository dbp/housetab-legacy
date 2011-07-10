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

{-import            Control.Concurrent (threadDelay) -- for debugging-}

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
import            Lib
import            Mail (mailActivation, mailEmailChange, mailResetPassword)
import            Views.Site
import            Views.Result
import            Views.Account
import            Views.Person
import            Controllers.Form
import            Controllers.Tutorial

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
                         {-, ("tutorialOn", if (tutorialActive user) then identitySplice else blackHoleSplice)
                         , ("tutorialOff", if (tutorialActive user) then blackHoleSplice else identitySplice)-}
                         , ("tutorialOn", tutorialTest True)
                         , ("tutorialOff", tutorialTest False)
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
             redirect $ fromMaybe "/entries#navbar" r

-- Make sure you have a 'password' field in there
loginH :: Application ()
loginH = renderHT "account/login"

loginFailure :: Application ()
loginFailure = heistLocal (bindSplice "failure" (textSplice "yes")) $ renderHT "account/login"

loginSuccess :: Application ()
loginSuccess = do u <- currentUser
                  recalculateTotals (fromJust u)
                  t <- getFromSession "tutorial-step"
                  case t of
                    Nothing -> when (tutorialActive (fromJust u)) $ setInSession "tutorial-step" "1"
                    _ -> return ()
                  redirTo

unDoc (Doc fs) = fs


signupH :: Application ()
signupH = do
  r <- eitherSnapForm signupForm "signup-form"
  case r of
    Left splices' -> 
      heistLocal (bindSplices splices') $ renderHT "account/signup_form"
    Right creds -> do  
      token <- liftIO $ getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
      let user = (makeUser token creds) { tutorialActive = True }
      au <- saveAuthUser (authUser user, additionalUserFields user) 
      case au of
        Nothing -> redirect "/signup" -- something went wrong in db... oops
        Just au' -> do --setSessionUserId $ userId au'
                       mailActivation token (accountName user) (accountEmail user)
                       renderHT "account/signup_success"
                   
signupForm :: SnapForm Application T.Text HeistView SignupCreds
signupForm = mkSignupCreds
    <$> input "name" Nothing `validate` nonEmpty `validate` noSpaces `validate` uniqueUser <++ errors
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
  either (const $ redirect "/") (const $ redirect "/settings") res 

passwordForm :: SnapForm Application T.Text HeistView NewPassword
passwordForm = (`validate` matchingPasswords) $ (<++ errors) $ NewPassword
    <$> input "password" Nothing  `validate` nonEmpty <++ errors 
    <*> input "confirm"  Nothing  `validate` nonEmpty <++ errors 
  where matchingPasswords = check "Passwords do not match." $ \(NewPassword p1 p2) -> p1 == p2
        mkPW p1 p2 = NewPassword p1 p2

resetPasswordH :: Application ()
resetPasswordH = do
  mtoken        <- getParam "token"
  maccountName  <- getParam "account"  -- will this work?: liftM fromJust $ getParam "account"
  case (mtoken,maccountName) of
    (Just token, Just accountName) -> do
      r <- eitherSnapForm passwordForm "password-reset-form"
      case r of
        Left splices' -> heistLocal (bindSplices splices') $ renderHT "account/reset"
        Right (NewPassword pw _) -> do  
          muser <- withDB $ findOne (select ["accountName" =: accountName, "accountReset" =: Just token] "users")
          case muser of
            Left _ -> renderHT "account/reset_error"
            Right user' -> 
              case user' of
                Nothing -> renderHT "account/reset_error"
                Just user -> do
                  {-liftIO $ putStrLn "Building user"-}
                  let n = fmap dropReset $ buildUser $ 
                            (,) <$> (fmap (setPass (B8.pack pw)) (docToAuthUser user)) 
                                <*> (Just user)
                  maybe (renderHT "account/reset_error") 
                        (\u -> do saveAuthUser (authUser u, additionalUserFields u)
                                  renderHT "account/reset_success") n   
    _ -> renderHT "account/reset_error"
     
 where dropReset u = u { accountReset = Nothing }
       setPass p au = au {userPassword = Just $ ClearText p}

forgotPasswordH :: Application ()
forgotPasswordH = do
  account <- getParam "account"
  email <- getParam "email"
  case account of
    a | a == Just "" || a == Nothing -> 
      case email of
        Just "" -> renderHT "account/forgot_empty"
        Nothing -> renderHT "account/forgot_empty"
        Just em -> do res <- resetByEmail em
                      case res of
                        Nothing -> renderHT "account/forgot_invalid"
                        Just (a,em,token) -> do
                          mailResetPassword a em token
                          renderHT "account/forgot_success"
    Just acc -> do res <- resetByAccount acc
                   case res of
                     Nothing -> renderHT "account/forgot_invalid"
                     Just (a,em,token) -> do
                       mailResetPassword a em token
                       renderHT "account/forgot_success"

changeSettingsForm :: User -> SnapForm Application T.Text HeistView NewSettings
changeSettingsForm user = mkSettings
    <$> input "current" Nothing `validate` (checkPassword user) <++ errors
    <*> ((`validate` matchingPasswords) $ (<++ errors) $ NewPassword
          <$> input "password" Nothing <++ errors  -- done here so that they can be blank
          <*> input "confirm"  Nothing <++ errors )
    <*> input "email" Nothing `validate` validEmail <++ errors
  where mkSettings _ (NewPassword p _) e  = NewSettings (B8.pack p) (B8.pack e)
        matchingPasswords = check "Passwords do not match." $ \(NewPassword p1 p2) -> p1 == p2
        mkPW p1 p2 = NewPassword p1 p2
  

changeSettingsH :: User -> Application ()
changeSettingsH user = do
  r <- eitherSnapForm (changeSettingsForm user) "settings-form"
  case r of
    Left splices' -> 
      heistLocal (bindSplices splices') $ renderHT "account/change_settings"
    Right (NewSettings pass email) -> do
      au <- case pass of
        "" -> return $ Just (authUser user) -- no password, so they aren't trying to change it
        _  -> saveAuthUser ((authUser user) {userPassword = Just (ClearText pass)}, additionalUserFields user) 
      case (email,au) of
        (e,_) | e == accountEmail user -> renderHT "account/change_password" -- or nothing, but it doesnt really matter
        (_,Just auth) -> do 
                token <- liftIO $ getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
                saveAuthUser (auth, additionalUserFields (user { accountEmailChange = Just (token, email)})) 
                mailEmailChange token (accountName user) email
                renderHT "account/change_email"
        _ -> redirect "/settings"

changeEmailH :: Application ()
changeEmailH = do
  token         <- getParam "token"
  maccountName  <- getParam "account"
  -- this is not the best user experience - better to present a message saying the token is missing, probably
  guard $ all isJust [token,maccountName]
  let accountName = fromJust maccountName
  u <- liftM buildUser $ getUserExternal (EUId (M.fromList [("accountName", [accountName])]))
  case u of
    Nothing -> return ()
    Just user | (fmap fst (accountEmailChange user)) == token -> do
      saveAuthUser (authUser user, additionalUserFields (user { accountEmail =  snd $ fromJust $ accountEmailChange user
                                                              , accountEmailChange = Nothing }))
      redirect "/settings" 
    _ -> redirect "/settings" 
    

deleteAccountH :: User -> Application ()
deleteAccountH user = do
  deleteAccount user
  performLogout
  redirect "/"
                           