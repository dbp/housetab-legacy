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


mailActivation token account emails = do
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ putStrLn "Sending activation email [fake]"
  -- liftIO $ mapM (postmark postmarkToken "messages@housetab.org" "Welcome to HouseTab. Now activate your account." "activation" (BS.concat $ msg server portNum)) emails
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

resetPassword email token = undefined