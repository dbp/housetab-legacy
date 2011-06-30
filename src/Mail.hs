{-# LANGUAGE OverloadedStrings #-}

module Mail where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec as A
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import Network.HTTP hiding (getRequest)
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI (parseURI)
import Secrets (postmarkToken)
import Control.Monad.Trans

import Network.Mail.Postmark

import Snap.Types hiding (POST)


mailActivation token account email = do
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  {-liftIO $ putStrLn "Sending activation email [fake]"-}
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" "Welcome to HouseTab. Now activate your account." "activation" (BS.concat $ msg server portNum)) [email]
    where msg s p = ["Welcome to HouseTab. "
                  ,"All you need to do to complete your registration is activate your account by clicking on the link below.\n\n"
                  ,"Please activate your account by visiting "
                  ,"http://"
                  ,s
                  ,if p /= 80 then (B8.pack $ ':' : (show p)) else ""
                  ,"/activate?account="
                  ,account
                  ,"&token="
                  ,token
                  ," .\n\nThanks! - The HouseTab Team"]

mailEmailChange token account email = do 
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  {-liftIO $ putStrLn "Sending email change email [fake]"-}
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" "Confirm your new email address on HouseTab." "emailchange" (BS.concat $ msg server portNum)) [email]
    where msg s p = ["You just changed your email account on HouseTab to this one. "
                  ,"To confirm that this is indeed your email account, please visit the following link.\n\n"
                  ,"http://"
                  ,s
                  ,if p /= 80 then (B8.pack $ ':' : (show p)) else ""
                  ,"/changeemail?account="
                  ,account
                  ,"&token="
                  ,token
                  ," .\n\nIf you did not do this, or don't know what HouseTab is, don't click on the link, and we won't bother you again."
                  ,"\n\nThanks! - The HouseTab Team"]


mailResetPassword account email token = do 
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  {-liftIO $ putStrLn "Sending password reset email [fake]"-}
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" "Reset your password on HouseTab." "reset" (BS.concat $ msg server portNum)) [email]
    where msg s p = ["You just requested to reset your password on HouseTab. "
                  ,"To do that, please visit the following link.\n\n"
                  ,"http://"
                  ,s
                  ,if p /= 80 then (B8.pack $ ':' : (show p)) else ""
                  ,"/reset?account="
                  ,account
                  ,"&token="
                  ,token
                  ," .\n\nIf you did not do this, don't click on the link, and your password will remain what it was before."
                  ,"\n\nThanks! - The HouseTab Team"]