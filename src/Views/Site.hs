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
import Data.List (intercalate)
import Data.List.Split (splitEvery)

import Heist.Splices.Async
import Views.Entry

import Application 

moneyShow m = (if m < 0 then "-$" else "$") ++ ((reverse . intercalate "," . splitEvery 3 . reverse . show . abs . floor) m)


boxField :: Monad m => Splice m
boxField = boxFieldGen "box-field" id []

boxFieldMulti :: Monad m => Splice m
boxFieldMulti = boxFieldGen "box-field-multi" countSelected [X.Element "div" [("class","close")] [X.TextNode "X"]]
  where countSelected t = T.concat [T.pack (show (if T.length t == 0 then 0 else length (T.splitOn "," t)))
                                   ," selected."
                                   ]

boxFieldGen :: Monad m => T.Text -> (T.Text -> T.Text) -> [X.Node] -> Splice m
boxFieldGen typ sel extra = do node <- getParamNode
                               case X.getAttribute "name" node of
                                 Nothing -> return [] -- without a name, inputs are useless
                                 Just name -> do
                                   let klass = T.concat [typ, " ", (fromMaybe "" $ X.getAttribute "class" node)]
                                   let value = fromMaybe "" $ X.getAttribute "value" node
                                   let display = sel $ fromMaybe "" $ X.getAttribute "display" node
                                   let children = [ X.Element "input" [("type","hidden"),("name",name),("value",value)] []
                                                  , X.Element "div" [("class","display")] [X.TextNode display]
                                                  , X.Element "div" [("class","box"),("style","display:none;")] (extra ++ (X.elementChildren node))
                                                  ]
                                   return [X.setAttribute "class" klass $ X.Element "div" (filter ((flip notElem ["name","value"]).fst) $ X.elementAttrs node) children]

boxOption :: Monad m => Splice m
boxOption = do node <- getParamNode
               case X.getAttribute "value" node of
                 Nothing -> return [] -- without a value, this isn't worth much
                 Just value -> do
                   let klass = T.concat ["option ",(fromMaybe "" $ X.getAttribute "class" node)]
                   let attributes = ("class", klass) : (filter ((flip notElem ["class","value"]).fst) $ X.elementAttrs node)
                   return [X.setAttribute "data-box-value" value $ X.Element "div" attributes (X.elementChildren node)]


moreBox :: Monad m => Splice m
moreBox = do node <- getParamNode
             let children = X.elementChildren node
             let more = case filter isMore children of
                          (x:xs) -> [X.Element "div" [("class","more"), ("style","display:none;")] (X.elementChildren x)]
                          _ -> []
             let showing = X.Element "div" [("class","showing")] $ filter (not.isMore) children
             let klass = T.concat ["more-box ",(fromMaybe "" $ X.getAttribute "class" node)]
             return [X.setAttribute "class" klass $ X.Element "div" (X.elementAttrs node) (showing:more)] 
    where isMore (X.Element tag _ _) = tag == "more"
          isMore _ = False


-- | t his splice shows it's children if the blank attribute is not blank :)
showContent :: Monad m => Splice m
showContent = do node <- getParamNode
                 case X.getAttribute "blank" node of
                   Just "" -> return []
                   Nothing -> return []
                   _ -> return $ X.elementChildren node

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
                  , ("box-field-multi", boxFieldMulti)
                  , ("box-option", boxOption)
                  , ("categories", categories)
                  , ("catName", categoryName)
                  , ("catImage", categoryImage)
                  , ("more-box", moreBox)
                  , ("show", showContent)
                  ] ++ heistAsyncSplices