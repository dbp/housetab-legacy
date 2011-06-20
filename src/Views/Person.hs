{-# LANGUAGE OverloadedStrings #-}

module Views.Person where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe)
import qualified  Data.ByteString.Char8 as B8
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Snap.Auth

import            Models.Person

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

getPeopleSplices htid = do people <- getHouseTabPeople $ emptyAuthUser {userId = Just (UserId (objid2bs htid))}
                           return [("people", renderPeople people)]