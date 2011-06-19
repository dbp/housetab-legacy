{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Person where
  
import            Snap.Extension
import qualified  Snap.Extension.DB.MongoDB as DB
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import qualified  Snap.Auth as A
import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Typeable
import            Data.Maybe (catMaybes, listToMaybe, isNothing)
import            Data.List.Split (splitOn)
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader
import            Application

import            Models.Entry

data Share = Share { sDate :: Date, sValue :: Double }
  deriving (Show, Read, Eq, Typeable, Ord)
instance Val Share where
    val (Share date value) = Doc ["date" =: date, "value" =: value]
    cast' (Doc fields) = do
      d <- B.lookup "date"    fields
      v <- B.lookup "value" fields
      return (Share d v)
    cast' _ = Nothing

data Person = Person { pId    :: Maybe ObjectId
                     , pHTId  :: ObjectId
                     , pName  :: BS.ByteString
                     , pShares :: [Share]}
                     deriving (Show, Eq, Typeable, Ord)

getHouseTabPeople :: A.AuthUser -> Application [Person]
getHouseTabPeople au = do
  case A.userId au of
    Just (A.UserId uid) -> do c <- DB.withDB $ DB.find $ DB.select ["htid" =: bs2objid uid] "people"
                              {-liftIO $ putStrLn $ show uid-}
                              case c of
                                Left err -> return [] -- some error occured
                                Right curs -> do
                                  docs <- DB.withDB $ DB.rest curs
                                  {-liftIO $ putStrLn $ show docs-}
                                  case docs of
                                    Left _ -> return [] -- an error occured
                                    Right es -> do let ps = map (cast' . Doc) es
                                                   {-liftIO $ putStrLn $ show ps-}
                                                   return $ catMaybes ps
    Nothing -> return []

getHouseTabPerson :: ObjectId -> Application (Maybe Person)
getHouseTabPerson id' = do person' <- DB.withDB $ DB.findOne $ DB.select ["_id" =: id'] "people"
                           case person' of
                             Left _ -> return Nothing
                             Right person -> return $ (cast' . Doc) =<< person

saveHouseTabPerson :: Person -> Application ()
saveHouseTabPerson person = do DB.withDB $ DB.save "people" (processNew $ unDoc $ val person)
                               return ()
  where unDoc (Doc fields) = fields
        processNew fields = if isNothing (B.lookup "_id" fields :: Maybe ObjectId) then exclude ["_id"] fields else fields 
        



instance Val Person where
    val (Person id htid name shares) = Doc ["_id" =: id, "htid" =: htid, "name" =: name, "shares" =: shares]
    cast' (Doc fields) = do
      i <- B.lookup "_id"    fields
      h <- B.lookup "htid"   fields
      n <- B.lookup "name"   fields
      s <- B.lookup "shares" fields
      return (Person i h n s)
    cast' _ = Nothing
