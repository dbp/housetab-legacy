{-# LANGUAGE OverloadedStrings #-}

module Views.Person where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe, maybeToList)
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Snap.Auth
import qualified  Data.Map as M

import            Models.Person
import            Application

renderPersonChild :: Monad m => Person -> Splice m
renderPersonChild person = runChildrenWith (renderPerson person)

renderPerson :: Monad m => Person -> [(T.Text, Splice m)]
renderPerson (Person pid htid name shares) =
  map ((\(a,b) -> (a, textSplice b)))
   [("personId",    TE.decodeUtf8 (maybe "" objid2bs pid))
   ,("htid",        TE.decodeUtf8 $ objid2bs htid)
   ,("personName",  TE.decodeUtf8 $ name)
   ]                       

renderPeople :: Monad m => [Person] -> Splice m
renderPeople people = mapSplices renderPersonChild people

lookupName :: Monad m => M.Map T.Text T.Text -> Splice m
lookupName people = do node <- getParamNode
                       case X.getAttribute "id" node of
                         Nothing -> return [] -- no id, so no name
                         Just id' -> return $ maybeToList $ fmap X.TextNode $ M.lookup id' people 

getPeopleSplices :: AuthUser -> Application [(T.Text, Splice Application)]
getPeopleSplices au = do people <- getHouseTabPeople au
                         return [("people", renderPeople people)
                                ,("lookupName", lookupName (personIdMap people))]