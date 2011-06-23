module Models.Site where

import Data.Bson  (ObjectId (Oid))
import            Data.Time.Clock
import            Data.Time.LocalTime

emptyObjectId = Oid 0 0


getLocalTime = do now  <- getCurrentTime
                  zone <- getCurrentTimeZone
                  return $ utcToLocalTime zone now