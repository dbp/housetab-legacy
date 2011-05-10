{-# LANGUAGE OverloadedStrings #-}

module Site
  ( site
  ) where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Trans (liftIO)
import            Data.Maybe
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Snap.Extension.Heist
import            Snap.Extension.Session.CookieSession
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers
import qualified  Data.Bson as B
import            Snap.Extension.DB.MongoDB hiding (index, label, find)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Time.Clock (getCurrentTime, diffUTCTime)

import            Text.Digestive.Types
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import            Text.Digestive.Validate

import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import            Data.List (null, sortBy, find)
import            System.Random (randomRIO)

import            Application
import            Account
import            State
import            Lib
import            Form
import qualified  Utils as U


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

logAccess :: String -> Application () -> Application ()
logAccess name action = do
  start <- liftIO $ getCurrentTime
  result   <- action
  u     <- getFromSession "accountName"
  let user = fromMaybe "Anonymous" u
  end   <- liftIO $ getCurrentTime
  let diff = fromRational $ toRational $ diffUTCTime end start
  withDBUnsafe $ insert "time"   ["page" =: name, "user" =: user, "time" =: (diff :: Double), "date" =: start]
  withDBUnsafe $ repsert (select ["page" =: name, "user" =: user] "access") ["$inc" =: ["hits" := (val (1 :: Int))]]
  return result

index :: Application ()
index = do  u <- currentUser
            let name = TE.decodeUtf8 $ maybe "No User" accountName u
            ifTop $ (heistLocal $ (bindString "user" name)) $ render "index"

errorP :: String -> Application ()
errorP msg = (heistLocal $ (bindString "message" (T.pack msg))) $ render "error"

renderPersonResult :: Monad m => (Person,Spent,Owes) -> Splice m
renderPersonResult (person,spent,owes) = do
  runChildrenWithText [("personName", TE.decodeUtf8 $ name person)
                      ,("personLetter", T.pack $ [letter person])
                      ,("personSpent",  T.pack $ show spent)
                      ,("personOwes",  T.pack $ show owes)]
                       
renderResult :: Monad m => Result -> Splice m
renderResult (Result people date) = mapSplices renderPersonResult people

renderEntry :: Monad m => HouseTabEntry -> Splice m
renderEntry (HouseTabEntry uid who what when howmuch whopays) = do
  runChildrenWithText [("index",       T.pack $ show uid)
                      ,("entryBy",     TE.decodeUtf8 who)
                      ,("entryWhat",   TE.decodeUtf8 what)
                      ,("entryDate",   T.pack $ show when)
                      ,("entryAmount", T.pack $ show howmuch)
                      ,("entryFor",    TE.decodeUtf8 whopays)
                      ]
                       
renderEntries :: Monad m => [HouseTabEntry] -> Splice m
renderEntries entries = mapSplices renderEntry entries

                     
entriesH :: User -> Application ()
entriesH user = do 
   (heistLocal $ (bindSplices splices)) $ render "entries"
     where splices = [ ("result",  (renderResult  $ currentResult user))
                     , ("entries", (renderEntries $ houseTabEntries user))
                     ]

addPerson :: User -> Application ()
addPerson user = do
   r <- eitherSnapForm (personForm Nothing) "add-person-form"
   case r of
       Left form' -> 
         heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
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
            heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
          Right person' -> do
            modPeople (U.findReplace ((== l).letter) person') user
            redirect "/entries"
      _ -> redirect "/entries"

addEntry :: User -> Application ()
addEntry user = do
         r <- eitherSnapForm (entryForm Nothing) "add-entry-form"
         case r of
             Left form' -> 
               heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
             Right entry' -> do
               id <- liftIO $ randomRIO (0,1000000)
               modEntries ([entry' {eid = id}] ++) user
               redirect "/entries"                    
 
editEntry :: User -> Application ()
editEntry user = 
  do i <- getParam "id"
     case i of
      Just uid' -> do
        let uid = read $ B8.unpack uid'
        r <- eitherSnapForm (entryForm (find ((== uid).eid) (houseTabEntries user))) "edit-entry-form" 
        case r of
          Left form' -> 
            heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
          Right entry' -> do
            modEntries (U.findReplace ((== uid).eid) entry') user
            redirect "/entries"
      Nothing -> redirect "/entries"

deleteEntry :: User -> Application ()
deleteEntry user = 
  do i <- getParam "id"
     case i of
      Just uid' -> do
        let uid = read $ B8.unpack uid'
        r <- eitherSnapForm deleteForm "delete-entry-form" 
        case r of
          Left form' -> 
            heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
          Right _ -> do
            modEntries (filter ((/= uid).eid)) user
            redirect "/entries"
      Nothing -> redirect "/entries"
      
site :: Application ()                 
site = route [ ("/",                    logAccess "index"           $ index)
             , ("/entries",             logAccess "entries"         $ ifTop $ requireUserBounce' entriesH)
             , ("/entries/add",         logAccess "entries-add"     $ requireUserBounce' $ addEntry)              
             , ("/entries/edit/:id",    logAccess "entries-edit"    $ requireUserBounce' $ editEntry)              
             , ("/entries/delete/:id",  logAccess "entries-delete"  $ requireUserBounce' $ deleteEntry)              
             , ("/people/add",          logAccess "people-add"      $ requireUserBounce' $ addPerson)
             , ("/people/edit/:letter", logAccess "people-edit"     $ requireUserBounce' $ editPerson)
             , ("/signup",              logAccess "signup"          $ method GET $ newSignupH)
             , ("/signup",              logAccess "signup-post"     $ method POST $ signupH)
             , ("/login",               logAccess "login"           $ method GET $ newSessionH ())
             , ("/login",               logAccess "login-post"      $ method POST $ loginHandler "password" Nothing newSessionH redirTo)
             , ("/logout",              logAccess "logout"          $ method GET $ logoutHandler redirTo)

             ]
       <|> serveDirectory "resources/static"
