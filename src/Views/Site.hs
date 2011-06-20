{-# LANGUAGE OverloadedStrings #-}

module Views.Site where
  
import Text.Templating.Heist
import Text.XmlHtml (childNodes)
import qualified Text.XmlHtml as X
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import Snap.Extension.Heist
import Data.ByteString (ByteString)
import Snap.Auth.Handlers
import Snap.Auth
import Snap.Extension.Heist
import Snap.Extension.DB.MongoDB
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)

import            Heist.Splices.Async

{-import Notification (notificationSplice)-}
import Application 

type BName = ByteString
type BValue = ByteString
type BShow = ByteString
boxField :: Monad m => Splice m
boxField = do node <- getParamNode
              case X.getAttribute "name" node of
                Nothing -> return [] -- without a name, inputs are useless
                Just name -> do
                  let klass = T.concat ["box-field ",(fromMaybe "" $ X.getAttribute "class" node)]
                  let value = fromMaybe "" $ X.getAttribute "value" node
                  let children = [ X.Element "input" [("type","hidden"),("name",name),("value",value)] []
                                 , X.Element "div" [("class","display"), ("style", "width: 200px; height:20px; border: 1px solid black;")] []
                                 , X.Element "div" [("class","box"),("style","display:none;")] (X.elementChildren node)
                                 ]
                  return [X.setAttribute "class" klass $ X.Element "div" (filter ((/= "name").fst) $ X.elementAttrs node) children]

boxOption :: Monad m => Splice m
boxOption = do node <- getParamNode
               case X.getAttribute "value" node of
                 Nothing -> return [] -- without a value, this isn't worth much
                 Just value -> do
                   let klass = T.concat ["option ",(fromMaybe "" $ X.getAttribute "class" node)]
                   let attributes = ("class", klass) : (filter ((flip notElem ["name","class"]).fst) $ X.elementAttrs node)
                   return [X.setAttribute "data-box-value" value $ X.Element "div" attributes (X.elementChildren node)]

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
                  , ("box-field", boxField)
                  , ("box-option", boxOption)
                  ] ++ heistAsyncSplices