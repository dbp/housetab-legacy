{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Entry where

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

data HouseTabEntry = HouseTabEntry
    { eId      :: Maybe ObjectId
    , eHTId    :: ObjectId
    , eWho     :: ObjectId
    , eWhat    :: BS.ByteString
    , eWhen    :: Date
    , eHowmuch :: Double
    , eWhopays :: [ObjectId]
    }
    deriving (Show, Eq, Typeable)


getHouseTabEntries :: A.AuthUser -> Application [HouseTabEntry]
getHouseTabEntries au = do
  case A.userId au of
    Just (A.UserId uid) -> do c <- DB.withDB $ DB.find $ DB.select ["htid" =: bs2objid uid] "entries"
                              case c of
                                Left _ -> return [] -- some error occured
                                Right curs -> do
                                  docs <- DB.withDB $ DB.rest curs
                                  case docs of
                                    Left _ -> return [] -- an error occured
                                    Right es -> return $ catMaybes $ map (cast' . Doc) es
    Nothing -> return []

getHouseTabEntry :: BS.ByteString -> Application (Maybe HouseTabEntry)
getHouseTabEntry id' = do entry' <- DB.withDB $ DB.findOne $ DB.select ["_id" =: bs2objid id'] "entries"
                          case entry' of
                            Left _ -> return Nothing
                            Right entry -> return $ (cast' . Doc) =<< entry

saveHouseTabEntry :: HouseTabEntry -> Application ()
saveHouseTabEntry entry = do DB.withDB $ DB.save "entries" (processNew $ unDoc $ val entry)
                             return ()
  where unDoc (Doc fields) = fields
        processNew fields = if isNothing (B.lookup "_id" fields :: Maybe ObjectId) then exclude ["_id"] fields else fields 


instance Val HouseTabEntry where
    val (HouseTabEntry id' htid who what when howmuch whopays) = 
      Doc ["_id" =: id', "htid" =: htid, "who" =: who, "what" =: what, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
    cast' (Doc fields) = do
      id'     <- B.lookup "_id"     fields
      htid    <- B.lookup "htid"    fields
      who     <- B.lookup "who"     fields
      what    <- B.lookup "what"    fields
      when    <- B.lookup "when"    fields
      howmuch <- B.lookup "howmuch" fields
      whopays <- B.lookup "whopays" fields      
      return (HouseTabEntry id' htid who what when howmuch whopays)
    cast' _ = Nothing

-- stolen from cgi:
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

data Date = Date { year :: Integer
                 , month :: Integer
                 , day :: Integer}
                 deriving (Eq, Typeable, Ord)
   
instance Show Date where
    show (Date year month day) = (show year) ++ "." ++ (show month) ++ "." ++ (show day)
instance Read Date where
    readsPrec _ value = pd $ splitOn (".") value
        where pd (year:month:day:[]) = mkDate (maybeRead year) (maybeRead month) (maybeRead day)
              pd _ = []
              mkDate (Just y) (Just m) (Just d) = [(Date y m d, "")]
              mkDate _ _ _ = []
instance Val Date where
    val (Date year month day) = Doc ["year" =: year, "month" =: month, "day" =: day] 
    cast' (Doc fields) = do
      y <- B.lookup "year"  fields
      m <- B.lookup "month" fields
      d <- B.lookup "day"   fields
      return (Date y m d)
    cast' _ = Nothing

emptyDate = Date 0 0 0
