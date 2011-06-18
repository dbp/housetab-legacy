{-# LANGUAGE OverloadedStrings #-}

module Controllers.Site
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
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate

import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import qualified  Text.XmlHtml as X

import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import            Data.List (null, sortBy, find)
import            System.Random (randomRIO)

import            Application
import            Lib
import qualified  Utils as U

import            Heist.Splices.Async
import            Snap.Logging.MongoDB

import            Views.Site
import            Controllers.Account
import            Controllers.Entry
import            Controllers.Person


site :: Application ()                 
site = logAccess $ 
       route [ ("/",                    ifTop $ renderHT "index")
             , ("/entries",             ifTop $ requireUserBounce' entriesH)
             , ("/entries/add",         requireUserBounce' $ addEntry)              
             {-, ("/entries/edit/:id",    requireUserBounce' $ editEntry)              
             , ("/entries/delete/:id",  requireUserBounce' $ deleteEntry)              -}
             , ("/people/:person/share/add",    requireUserBounce' $ addShare)
             , ("/people/add",          requireUserBounce' $ addPerson)
             {-, ("/people/edit/:letter", requireUserBounce' $ editPerson)-}
             , ("/signup",              method GET $ newSignupH)
             , ("/signup",              method POST $ signupH)
             , ("/login",               method GET $ newSessionH ())
             , ("/login",               method POST $ loginHandler "password" Nothing newSessionH redirTo)
             , ("/logout",              method GET $ logoutHandler redirTo)
             , ("/activate",            activateAccountH)
             ]
       <|> serveDirectory "resources/static"