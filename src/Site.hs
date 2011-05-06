{-# LANGUAGE OverloadedStrings #-}

module Site
  ( site
  ) where

import            Control.Applicative
import            Control.Monad
import            Data.Maybe
import qualified  Data.Text.Encoding as T
import            Snap.Extension.Heist
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers

import            Application
import            Auth

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = do  u <- currentAuthUser
            let e = T.decodeUtf8 $ maybe "No User" id $ join $ fmap (userEmail.fst) u
            ifTop $ (heistLocal $ (bindString "user" e)) $ render "index"


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             , ("/signup",      method GET $ newSignupH)
             , ("/signup",      method POST $ signupH)
             , ("/login",       method GET $ newSessionH ())
             , ("/login",       method POST $ loginHandler "password" Nothing newSessionH redirHome)
             , ("/logout",      method GET $ logoutHandler redirHome)

             ]
       <|> serveDirectory "resources/static"
