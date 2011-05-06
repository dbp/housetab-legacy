{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module State where

import qualified  Snap.Extension.DB.MongoDB as DB
import qualified  Snap.Auth as A
import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Typeable
import            Data.Maybe (catMaybes)

data HouseTabEntry = HouseTabEntry
    { eid :: BS.ByteString
    , ewho  :: BS.ByteString
    , ewhat :: BS.ByteString
    , ewhen    :: Date
    , ehowmuch :: Double
    , ewhopays :: BS.ByteString
    }
    deriving (Show, Read, Eq, Typeable)

instance Val HouseTabEntry where
    val (HouseTabEntry id who what when howmuch whopays) = 
      Doc ["id" =: id, "who" =: who, "what" =: what, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
    cast' (Doc fields) = do
      id      <- B.lookup "id"      fields
      who     <- B.lookup "who"     fields
      what    <- B.lookup "what"    fields
      when    <- B.lookup "when"    fields
      howmuch <- B.lookup "howmuch" fields
      whopays <- B.lookup "whopays" fields      
      return (HouseTabEntry id who what when howmuch whopays)
    cast' _ = Nothing


data HouseTab = HouseTab { houseTabEntries :: [HouseTabEntry]
                         , houseTabPeople :: [Person]}
                         deriving (Show, Read, Eq, Typeable)

instance Val HouseTab where
    val (HouseTab entries people) = Doc ["entries" =: entries, "people" =: people] 
    cast' (Doc fields) = do
      e <- B.lookup "entries"  fields
      p <- B.lookup "people" fields
      return (HouseTab e p)
    cast' _ = Nothing

data Account = Account
    { aid :: BS.ByteString
    , aname :: BS.ByteString
    , aemails :: [BS.ByteString]
    , ahousetab :: HouseTab
    , acurrent :: Result
    , areset :: Maybe BS.ByteString -- reset token
    , aactivate :: Maybe BS.ByteString } -- activation token
    deriving (Show, Read, Eq, Typeable)

instance Val Account where
    val (Account id name emails housetab current reset activate) = 
      Doc ["_id" =: id, "name" =: name, "emails" =: emails, "housetab" =: housetab, "current" =: current, "reset" =: reset, "activate" =: activate] 
    cast' (Doc fields) = do
      id        <- B.lookup "_id"        fields
      name      <- B.lookup "name"      fields
      emails    <- B.lookup "emails"    fields
      housetab  <- B.lookup "housetab"  fields
      current   <- B.lookup "current"   fields
      reset     <- B.lookup "reset"     fields      
      activate  <- B.lookup "activate"  fields      
      return (Account id name emails housetab current reset activate)
    cast' _ = Nothing


data Date = Date { year :: Integer
                 , month :: Integer
                 , day :: Integer}
                 deriving (Show, Read, Eq, Typeable, Ord)
   
{-instance Show Date where
    show (Date year month day) = (show year) ++ "." ++ (show month) ++ "." ++ (show day)-}
instance Val Date where
    val (Date year month day) = Doc ["year" =: year, "month" =: month, "day" =: day] 
    cast' (Doc fields) = do
      y <- B.lookup "year"  fields
      m <- B.lookup "month" fields
      d <- B.lookup "day"   fields
      return (Date y m d)
    cast' _ = Nothing

data Percent = Percent Date Double
  deriving (Show, Read, Eq, Typeable, Ord)
instance Val Percent where
    val (Percent date percent) = Doc ["date" =: date, "percent" =: percent]
    cast' (Doc fields) = do
      d <- B.lookup "date"    fields
      p <- B.lookup "percent" fields
      return (Percent d p)
    cast' _ = Nothing
  


data Person = Person { name :: BS.ByteString
                     , letter :: Char
                     , percs :: [Percent]}
                     deriving (Show, Read, Eq, Typeable, Ord)
                     
instance Val Person where
    val (Person name letter percs) = Doc ["name" =: name, "letter" =: [letter], "percs" =: percs]
    cast' (Doc fields) = do
      n <- B.lookup "name"    fields
      l <- B.lookup "letter"  fields
      p <- B.lookup "percs" fields
      return (Person n (head l) p)
    cast' _ = Nothing

tuple (x':y':[]) = do
  x <- cast' x'
  y <- cast' y'
  return (x,y)

untuple (x,y) = [val x, val y]
    
tuple3 (x':y':z':[]) = do
  x <- cast' x'
  y <- cast' y'
  z <- cast' z'
  return (x,y,z)

untuple3 (x,y,z) = [val x, val y, val z]

type Spent = Double
type Owes = Double

data Result = Result {people :: [(Person, Spent, Owes)]
                     ,currentdate :: Date}
                     deriving (Show, Read, Eq, Typeable)

instance Val Result where
    val (Result p date) = Doc ["people" =: people, "currentdate" =: date]
     where people = map untuple3 p
    cast' (Doc fields) = do
      p <- liftM (catMaybes . (map tuple3)) $ B.lookup "people" fields
      d <- B.lookup "currentdate" fields
      return (Result p d)
    cast' _ = Nothing


emptyHouseTab = HouseTab [] []            
emptyDate = Date 0 0 0
emptyResult = Result [] emptyDate
emptyAccount = Account "" "" [] emptyHouseTab emptyResult Nothing Nothing

{-------------------------------------------------------------------------------
-- | Turn a page from the database into 'Account'
docToAccount :: Document -> Maybe Account
docToAccount v = do
  id <- DB.lookup "_id" v
  name <- DB.lookup "name" v
  salt <- DB.lookup "salt" v
  return emptyAuthUser
            { aid = uid 
            , aname = name
            , userPassword = Just $ Encrypted pass 
            , userSalt = Just salt
            , userActivatedAt = DB.lookup "activated_at" v
            , userSuspendedAt = DB.lookup "suspended_at" v
            , userPersistenceToken = DB.lookup "persistence_token" v
            , userCreatedAt = DB.lookup "created_at" v
            , userUpdatedAt = DB.lookup "updated_at" v
            , userCurrentLoginAt = DB.lookup "current_login_at" v
            , userLastLoginAt = DB.lookup "last_login_at" v
            , userCurrentLoginIp = DB.lookup "current_login_ip" v
            , userLastLoginIp = DB.lookup "last_login_ip" v
            , userLoginCount = maybe 0 id $ DB.lookup "login_count" v
            , userFailedLoginCount = maybe 0 id $ DB.lookup "failed_login_count" v
            }


------------------------------------------------------------------------------
-- | Turn an 'AuthUser' into a 'Document' ready to be commited to DB.
authUserToDoc :: AuthUser -> Document
authUserToDoc usr = fields'
  where
    fields' = foldr step [] fields
    step x acc = maybe acc (: acc) x
    decidePass (Encrypted x) = Just ("password" =: x)
    decidePass _ = error "Can't save user without a proper password set"
    fields = 
      [ userId usr >>= return . ("_id" =:)    -- only if present
      , userCreatedAt usr >>= return . ("created_at" =:)  -- only if present
      , Just $ ("email" =: userEmail usr)
      , userPassword usr >>= decidePass
      , Just $ ("salt" =: userSalt usr)
      , Just $ ("activated_at" =: userActivatedAt usr)
      , Just $ ("suspended_at" =: userSuspendedAt usr)
      , Just $ ("persistence_token" =: userPersistenceToken usr)
      , Just $ ("current_login_at" =: userCurrentLoginAt usr)
      , Just $ ("last_login_at" =: userLastLoginAt usr)
      , Just $ ("current_login_ip" =: userCurrentLoginIp usr)
      , Just $ ("last_login_ip" =: userLastLoginIp usr)
      , Just $ ("login_count" =: userLoginCount usr)
      , Just $ ("failed_login_count" =: userFailedLoginCount usr)
      ]-}
