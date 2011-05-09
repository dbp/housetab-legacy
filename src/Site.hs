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
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers
import qualified  Data.Bson as B
import            Snap.Extension.DB.MongoDB hiding (index, label, find)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
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
   r <- eitherSnapForm addPersonForm "add-person-form"
   case r of
       Left form' -> 
         heistLocal (bindSplice "formdata" (formSplice form')) $ render "form"
       Right person' -> do
         let u' = user {houseTabPeople = (houseTabPeople user) ++ [person']}
         let u'' = u' {currentResult = run (houseTabPeople u') (houseTabEntries u')}
         saveAuthUser (authUser u'', additionalUserFields u'')
         redirect "/entries"


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
            modEntries (findReplace uid entry') user
            redirect "/entries"
      Nothing -> redirect "/entries"
    where findReplace uid val []     = val:[]
          findReplace uid val (x:xs) = if eid x == uid then val:xs else x : (findReplace uid val xs)

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
site = route [ ("/",                   index)
             , ("/entries",            ifTop $ requireUserBounce' entriesH)
             , ("/entries/add",        requireUserBounce' $ addEntry)              
             , ("/entries/edit/:id",   requireUserBounce' $ editEntry)              
             , ("/entries/delete/:id", requireUserBounce' $ deleteEntry)              
             , ("/people/add",         requireUserBounce' $ addPerson)
             , ("/signup",             method GET $ newSignupH)
             , ("/signup",             method POST $ signupH)
             , ("/login",              method GET $ newSessionH ())
             , ("/login",              method POST $ loginHandler "password" Nothing newSessionH redirTo)
             , ("/logout",             method GET $ logoutHandler redirTo)

             ]
       <|> serveDirectory "resources/static"
