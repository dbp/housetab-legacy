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
import            Data.Maybe (catMaybes, listToMaybe)
import            Data.List.Split (splitOn)

-- stolen from cgi:
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


data HouseTabEntry = HouseTabEntry
    { ewho     :: BS.ByteString
    , ewhat    :: BS.ByteString
    , ewhen    :: Date
    , ehowmuch :: Double
    , ewhopays :: BS.ByteString
    }
    deriving (Show, Read, Eq, Typeable)

instance Val HouseTabEntry where
    val (HouseTabEntry who what when howmuch whopays) = 
      Doc ["who" =: who, "what" =: what, "when" =: when, "howmuch" =: howmuch, "whopays" =: whopays] 
    cast' (Doc fields) = do
      who     <- B.lookup "who"     fields
      what    <- B.lookup "what"    fields
      when    <- B.lookup "when"    fields
      howmuch <- B.lookup "howmuch" fields
      whopays <- B.lookup "whopays" fields      
      return (HouseTabEntry who what when howmuch whopays)
    cast' _ = Nothing


{-data HouseTab = HouseTab { houseTabEntries :: [HouseTabEntry]
                         , houseTabPeople :: [Person]}
                         deriving (Show, Read, Eq, Typeable)

instance Val HouseTab where
    val (HouseTab entries people) = Doc ["entries" =: entries, "people" =: people] 
    cast' (Doc fields) = do
      e <- B.lookup "entries"  fields
      p <- B.lookup "people" fields
      return (HouseTab e p)
    cast' _ = Nothing-}

{-data Account = Account
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
      id        <- B.lookup "_id"       fields
      name      <- B.lookup "name"      fields
      emails    <- B.lookup "emails"    fields
      housetab  <- B.lookup "housetab"  fields
      current   <- B.lookup "current"   fields
      reset     <- B.lookup "reset"     fields      
      activate  <- B.lookup "activate"  fields      
      return (Account id name emails housetab current reset activate)
    cast' _ = Nothing-}


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

emptyDate = Date 0 0 0
emptyResult = Result [] emptyDate

