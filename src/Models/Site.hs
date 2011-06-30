module Models.Site where

import Data.Bson  (ObjectId (Oid))
import            Data.Time.Clock
import            Data.Time.LocalTime
import            Data.Bson hiding (lookup)


emptyObjectId = Oid 0 0


getLocalTime = do now  <- getCurrentTime
                  zone <- getCurrentTimeZone
                  return $ utcToLocalTime zone now
                  
                  
tuple (x':y':[]) = do
  x <- cast' x'
  y <- cast' y'
  return (x,y)

untuple (x,y) = [val x, val y]
  