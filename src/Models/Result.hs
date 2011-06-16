{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Result where
  
import            Snap.Extension
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
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader
import            Application

import            Models.Entry
import            Models.Person

    
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

emptyResult = Result [] emptyDate
