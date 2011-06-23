{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Models.History where
  
import            Snap.Extension
import qualified  Snap.Extension.DB.MongoDB as DB
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import qualified  Snap.Auth as A
import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Time.LocalTime
import            Data.Word
import            Data.Time.Clock
import            Data.Time.LocalTime

import            Data.Typeable
import            Data.Maybe (catMaybes, listToMaybe)
import            Data.List.Split (splitOn)
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader

import            Application

import            Models.Entry
import            Models.Site

data Change a = Change { changeOld :: a
                       , changeNew :: Maybe a
                       } deriving (Show, Eq, Typeable)

-- While these mimic the real entries, the id's are unique. This data redundancy allows the history to both exist completely independently from the real data (which is important as this will be the user's primary fail-safe), but also to make it easy to get rid of the whole history quickly and easily, which is important from the perspective of not being creepy.   
data History = 
             -- this is the same as a normal entry...
               Add { haId       :: Maybe ObjectId
                   , haHTId     :: ObjectId
                   , haDate     :: UTCTime
                   , haWho      :: ObjectId
                   , haWhat     :: BS.ByteString
                   , haCategory :: BS.ByteString
                   , haWhen     :: Date
                   , haHowmuch  :: Double
                   , haWhopays  :: [ObjectId]
                   }
              -- this is like a normal entry, but each field is marked whether it changed.
            | Edit { heId       :: Maybe ObjectId
                   , heHTId     :: ObjectId
                   , heDate     :: UTCTime
                   , heWho      :: Change ObjectId
                   , heWhat     :: Change BS.ByteString
                   , heCategory :: Change BS.ByteString
                   , heWhen     :: Change Date
                   , heHowmuch  :: Change Double
                   , heWhopays  :: Change [ObjectId]
                   }
              -- this is just like a normal entry, showing what it was when it was deleted
            | Delete { hdId       :: Maybe ObjectId
                     , hdHTId     :: ObjectId
                     , hdDate     :: UTCTime
                     , hdWho      :: ObjectId
                     , hdWhat     :: BS.ByteString
                     , hdCategory :: BS.ByteString
                     , hdWhen     :: Date
                     , hdHowmuch  :: Double
                     , hdWhopays  :: [ObjectId]
                     }
        deriving (Show, Eq, Typeable)

unDoc (Doc fields) = fields

trackAdd :: HouseTabEntry -> Application ()
trackAdd entry = do now <- liftIO getCurrentTime
                    DB.withDB $ DB.insert "history" (B.merge ["type" =: ("add" :: String), "date" =: now] 
                      $ B.exclude ["_id"] $ unDoc $ val entry)
                    return ()
  where unDoc (Doc fields) = fields

trackEdit :: HouseTabEntry -> HouseTabEntry -> Application ()
trackEdit (HouseTabEntry oid' ohtid owho owhat ocat owhen ohowmuch owhopays) 
          (HouseTabEntry nid' nhtid nwho nwhat ncat nwhen nhowmuch nwhopays) = do
    now <- liftIO getCurrentTime
    let edit = Edit Nothing ohtid now (mkC owho nwho) (mkC owhat nwhat) (mkC ocat ncat) 
                    (mkC owhen nwhen) (mkC ohowmuch nhowmuch) (mkC owhopays nwhopays)
    DB.withDB $ DB.insert "history" (unDoc $ val edit)
    return ()
  where mkC o n = if o == n then Change o Nothing else Change o (Just n)

trackDelete :: HouseTabEntry -> Application ()
trackDelete entry = do now <- liftIO getCurrentTime
                       DB.withDB $ DB.insert "history" (B.merge ["type" =: ("delete" :: String), "date" =: now] 
                        $ B.exclude ["_id"] $ unDoc $ val entry)
                       return ()

getHistory :: Word32 -> A.AuthUser -> Application [History]
getHistory page au = 
  case A.userId au of
    Just (A.UserId uid) -> 
      do c <- DB.withDB $ DB.find (DB.select ["htid" =: bs2objid uid] "history") { DB.limit = 30, 
                                                                                   DB.skip = page * 30, 
                                                                                   DB.sort = ["date" =: (-1 :: Int)]}
         case c of
           Left _ -> return [] -- some error occured
           Right curs -> do
             docs <- DB.withDB $ DB.rest curs
             case docs of
               Left _ -> return [] -- an error occured
               Right es -> return $ catMaybes $ map (cast' . Doc) es
    Nothing -> return []

instance Val a => Val (Change a) where
  val (Change old new) = Doc ["o" =: old, "n" =: new]
  cast' (Doc fields) = do
    old <- B.lookup "o" fields
    new <- B.lookup "n" fields
    return $ Change old new
    
instance Val History where
  val (Add id' htid date who what category when howmuch whopays) = 
    Doc ["_id" =: id', "htid" =: htid, "type" =: ("add" :: String), "date" =: date, "who" =: who, "what" =: what, 
         "category" =: category, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
  val (Edit id' htid date who what category when howmuch whopays) = 
    Doc ["_id" =: id', "htid" =: htid, "type" =: ("edit" :: String), "date" =: date, "who" =: who, "what" =: what,  
         "category" =: category, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
  val (Delete id' htid date who what category when howmuch whopays) = 
    Doc ["_id" =: id', "htid" =: htid, "type" =: ("delete" :: String), "date" =: date, "who" =: who, "what" =: what, 
         "category" =: category, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
    
  cast' (Doc fields) = do
    typ <- B.lookup "type"     fields
    case typ of
      -- need to do these separately because they are casting to different types.
      ("edit" :: String) -> do
        id'      <- B.lookup "_id"      fields
        htid     <- B.lookup "htid"     fields
        date     <- B.lookup "date"     fields
        who      <- B.lookup "who"      fields
        what     <- B.lookup "what"     fields
        category <- B.lookup "category" fields
        when     <- B.lookup "when"     fields
        howmuch  <- B.lookup "howmuch"  fields
        whopays  <- B.lookup "whopays"  fields      
        return (Edit id' htid date who what category when howmuch whopays)
      otherwise -> do
        id'      <- B.lookup "_id"      fields
        htid     <- B.lookup "htid"     fields
        date     <- B.lookup "date"     fields
        who      <- B.lookup "who"      fields
        what     <- B.lookup "what"     fields
        category <- B.lookup "category" fields
        when     <- B.lookup "when"     fields
        howmuch  <- B.lookup "howmuch"  fields
        whopays  <- B.lookup "whopays"  fields
        case typ of
          ("add" :: String)    -> return (Add id' htid date who what category when howmuch whopays)
          ("delete" :: String) -> return (Delete id' htid date who what category when howmuch whopays)
  cast' _ = Nothing
    

