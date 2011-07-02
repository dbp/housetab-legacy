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
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" (BS.concat ["Welcome to HouseTab, ", account, ". Now activate your account."]) "activation" (BS.concat $ msg server portNum)) [email]
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
                  ," .\n\nThanks! - The HouseTab Team"
                  ]

mailEmailChange token account email = do 
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  {-liftIO $ putStrLn "Sending email change email [fake]"-}
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" (BS.concat ["Confirm your new email address on HouseTab, ", account, "."]) "emailchange" (BS.concat $ msg server portNum)) [email]
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
                  ,"\n\nThanks! - The HouseTab Team"
                  ]


mailResetPassword account email token = do 
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  {-liftIO $ putStrLn "Sending password reset email [fake]"-}
  liftIO $ mapM (postmark postmarkToken "messages@housetab.org" (BS.concat ["Reset your password on HouseTab, ",account,"."]) "reset" (BS.concat $ msg server portNum)) [email]
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
                  ,"\n\nThanks! - The HouseTab Team"
                  ]
                  
         
mailNewHouseTab :: BS.ByteString -> Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO [Bool]                   
mailNewHouseTab server port account email token = do
  mapM (postmark postmarkToken "messages@housetab.org" "Welcome To The New HouseTab - And Important: Reset Your Password." "newhousetab" (BS.concat $ msg server port)) [email]
    where msg s p = [ "Dear HouseTab User "
                    , account
                    , ",\n\n"
                    , "You are receiving this email because at one point you signed up for an account at Housetab.org, and we need you to update your password due to a site redesign.\n\n"
                    , "Over the last few months, we have been building a new version of HouseTab, which features an entirely updated design and much improved functionality. If you haven’t checked out the site in a while, now might be a good time to take a look! If you use it all the time, we think you will agree that it’s a lot better!\n\n"
                    , "As part of the update we need you to reset your password. That’s because we switched all the HouseTab data to a more powerful platform, but we could not move your password, which is stored in an unreadable format. To reset your password, visit the following link:\n\n"
                    , "http://"
                    , s
                    , if p /= 80 then (B8.pack $ ':' : (show p)) else ""
                    , "/reset?account="
                    , account
                    , "&token="
                    , token
                    , "\n\n"
                    , "Feel free to keep the same password, but if you change it, be sure to let everyone in your house know!\n\n"
                    , "Beyond the design updates, one of the biggest improvements is that when you add, edit or delete an entry, those changes are recorded and displayed on the settings page by default. This history is a way to recover lost information if, for example, you delete an entry accidentally. However, we do not want to record this information if you do not want us to, so on that page, you will find a way to stop recording this history and delete all that we have recorded thus far.\n\n"
                    , "You will also notice that you now must select a category for all entries. All your old entries have been categorized as Miscellaneous, but feel free to recategorize them with tags like groceries or utilities. The categories will gradually become more important, as we provide more flexible ways to handle your entries.\n\n"
                    , "Thanks, and welcome to the new HouseTab,\n"
                    , "The HouseTab Team\n"
                    , "Position Studios\n"
                    , "http://positionstudios.com\n\n"
                    , "PS. As part of this change, we have gotten rid of the export / import functionality that you may have seen on the settings page. We are looking for other ways of implementing this (in a safer, more programmer friendly, way), but in case you decide that you do not want to be part of this new move and want to get your data out, the old site will remain up and functioning at http://old.housetab.org. However, if you continue to add entries to the old site, it is up to you to migrate that data over to the new HouseTab.\n"
                    ]