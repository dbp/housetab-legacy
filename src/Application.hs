{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses #-}

{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  ( Application
  , applicationInitializer
  ) where

import            Snap.Extension
import            Snap.Extension.Heist.Impl
import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB


------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    , cookieState   :: CookieSessionState
    , mongoState    :: MongoDBState
    }


------------------------------------------------------------------------------
instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

------------------------------------------------------------------------------
instance HasCookieSessionState ApplicationState where
    getCookieSessionState = cookieState

------------------------------------------------------------------------------
instance HasMongoDBState ApplicationState where
    getMongoDBState     = mongoState
    setMongoDBState s a = a { mongoState = s }
 
------------------------------------------------------------------------------
instance MonadAuth Application where
  authAuthenticationKeys = return ["accountName", "accountActivate"] -- accountActivate means that either they need the activation token to login (which will never happen) or the activation token needs to be Nothing (what happens when they activate). Sort af an ugly hack as snap-auth doesn't have a clear way of providing conditions that need to pass for a user to login. When that changes, this should go away.
  

------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.
applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist <- heistInitializer "resources/templates"
    cookie <- cookieSessionStateInitializer $ defCookieSessionState
              { csKeyPath = "config/site-key.txt" 
              , csCookieName = "housetab-session" }
    mongo <- mongoDBInitializer (host "127.0.0.1") 1 "housetab"
    return $ ApplicationState heist cookie mongo
