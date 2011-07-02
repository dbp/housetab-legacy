{-# LANGUAGE OverloadedStrings #-}

module Views.Person where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe, maybeToList, listToMaybe, mapMaybe)
import            Data.List (sortBy)
import            Data.Ord (comparing)
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Snap.Auth
import qualified  Data.Map as M
import            Control.Monad.Trans (liftIO)

import            Data.Time.Calendar
import            Data.Time.LocalTime
import            Data.Time.Clock

import            Models.Person
import            Models.Site
import            Models.Entry (dateToDay)
import            Application

import            Views.Site

renderPersonChild :: Monad m => Person -> Splice m
renderPersonChild person = runChildrenWith (renderPerson person)

renderPerson :: Monad m => Person -> [(T.Text, Splice m)]
renderPerson (Person pid htid name shares) =
   [("personId",             textSplice $ TE.decodeUtf8 (maybe "" objid2bs pid))
   ,("htid",                 textSplice $ TE.decodeUtf8 $ objid2bs htid)
   ,("personName",           textSplice $ TE.decodeUtf8 $ name)
   ,("personShares",         sharesSplice (reverse $ sortBy (comparing sDate) shares))
   ]
      

renderPeople :: Monad m => Day -> [Person] -> Splice m
renderPeople today people = mapSplices renderPersonChild people

personShareAsOf day (Person _ _ _ shares) = listToMaybe $ reverse $ takeWhile ((< day).fst) $ sortBy (\a b -> compare (fst a) (fst b)) $ map (\(Share d v) -> (dateToDay d,v)) shares 

getTotalShares today people = sum $ map snd $ mapMaybe (personShareAsOf today) people

lookupName :: Monad m => M.Map T.Text T.Text -> Splice m
lookupName people = do node <- getParamNode
                       case X.getAttribute "id" node of
                         Nothing -> return [] -- no id, so no name
                         Just id' -> return $ maybeToList $ fmap X.TextNode $ M.lookup id' people 

lookupPeopleShow :: {-Monad m => -}[Person] -> Splice Application
lookupPeopleShow people = do node <- getParamNode
                             case X.getAttribute "value" node of
                               Just t -> return [X.TextNode $ T.pack $ showPeople people $ mapMaybe (bs2objid . TE.encodeUtf8) $ T.splitOn "," t]
                               _ -> return []
                               
getPeopleSplices :: AuthUser -> Application [(T.Text, Splice Application)]
getPeopleSplices au = do people <- getHouseTabPeople au
                         today <- liftIO getLocalTime
                         return [("people", renderPeople (localDay today) people)
                                ,("lookupName", lookupName (personIdMap people))
                                ,("lookupPeopleShow", lookupPeopleShow people)
                                ]