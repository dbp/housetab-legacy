{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tutorial where

import            Snap.Types
import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.Heist
import            Text.Templating.Heist
import            Control.Monad.Trans
import            Data.Maybe (isJust)

import            Application
import            Models.Account
import            Models.Site
import            Views.Site
import            Views.Result

tutorialTest :: Bool -> Splice Application
tutorialTest on = do
  ta <- lift $ getFromSession "tutorial-step"
  if on then (if isJust ta then identitySplice else blackHoleSplice)
        else (if isJust ta then blackHoleSplice else identitySplice)

tutorialStep user old new =
  if not (tutorialActive user) then return () else do
    st <- getFromSession "tutorial-step"
    if st == Just old then setInSession "tutorial-step" new else return ()

       
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
