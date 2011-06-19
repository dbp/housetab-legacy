{-# LANGUAGE OverloadedStrings #-}
module Controllers.Form where
  
import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform

import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Data.Bson
import            Data.List.Split

import            Data.Text (Text)
import            Models.Entry
import            Control.Applicative
import            Control.Monad
import            Text.Templating.Heist

import            Application

emptyObjectId = Oid 0 0


nonEmpty :: Validator Application Text String
nonEmpty = check "String must not be empty." $ \s -> not $ null s
    
lenOne :: Validator Application Text String
lenOne = check "String must be a single character." $ \s -> length s == 1


validDate :: Validator Application Text Date
validDate = check "Must be a valid date, like 2011.2.25" $ \(Date y m d) -> and [y>1900,y<2100,m>=1,m<=12,d>=1,d<=31]

positive :: (Ord a, Num a) => Validator Application Text a
positive = check "Must be a positive number." $ \n -> n > 0

mongoObjectId :: Transformer Application Text String ObjectId
mongoObjectId = transformEither (\a -> maybe (Left "Invalid Object Id Specified") Right (bs2objid (B8.pack a)))

mongoObjectIdMany :: Transformer Application Text String [ObjectId]
mongoObjectIdMany = transformEither (\a -> maybe (Left "Invalid Object Id List Specified") Right (sequence $ map (bs2objid . B8.pack) (splitOn "," a)))