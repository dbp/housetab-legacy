{-# LANGUAGE OverloadedStrings #-}

module Views.Site where
  
import Text.Templating.Heist
import Text.XmlHtml (childNodes)
import Snap.Extension.Heist
import Data.ByteString (ByteString)
import Snap.Auth.Handlers
import Snap.Auth
import Snap.Extension.Heist
import Snap.Extension.DB.MongoDB
import Control.Monad.Trans (lift)

import            Heist.Splices.Async

{-import Notification (notificationSplice)-}
import Application 

--- the following two taken from https://github.com/mightybyte/snap-heist-splices which depends on unreleased version of snap
------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from an authenticated
-- user.
ifLoggedIn :: (MonadAuth m, MonadMongoDB m) => Splice m
ifLoggedIn = do
    node <- getParamNode
    res <- lift $ requireUser (return []) (return $ childNodes node)
    return res


------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from a user that is not
-- logged in.
ifGuest :: (MonadAuth m, MonadMongoDB m) => Splice m
ifGuest = do
    node <- getParamNode
    res <- lift $ requireUser (return $ childNodes node) (return [])
    return res

renderHT :: ByteString -> Application ()
renderHT = (heistLocal $ (bindSplices splices)) . render
  where splices = [ {-("notification", notificationSplice)
                    ,-} ("ifLoggedIn", ifLoggedIn)
                  , ("ifGuest", ifGuest)
                  ] ++ heistAsyncSplices