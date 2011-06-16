{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Person where
  
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

data Percent = Percent { pDate :: Date, pValue :: Double }
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
    


