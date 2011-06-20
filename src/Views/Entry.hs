{-# LANGUAGE OverloadedStrings #-}

module Views.Entry where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe)
import qualified  Data.ByteString.Char8 as B8
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Models.Entry

renderEntryChild :: Monad m => HouseTabEntry -> Splice m
renderEntryChild entry = runChildrenWith (renderEntry entry)

renderEntry :: Monad m => HouseTabEntry -> [(T.Text, Splice m)]
renderEntry (HouseTabEntry uid htid who what when howmuch whopays) =
  [("entryFor",    mapSplices runChildrenWithText (map ((:[]) . ((,) "value") . TE.decodeUtf8 . objid2bs) whopays))] ++
  (map ((\(a,b) -> (a, textSplice b)))
   [("index",       TE.decodeUtf8 (maybe "" objid2bs uid))
   ,("htid",        TE.decodeUtf8 $ objid2bs htid)
   ,("entryBy",     TE.decodeUtf8 $ objid2bs who)
   ,("entryWhat",   TE.decodeUtf8 what)
   ,("entryDate",   T.pack $ show when)
   ,("entryAmount", T.pack $ show howmuch)
   ])                       

renderEntries :: Monad m => [HouseTabEntry] -> Splice m
renderEntries entries = mapSplices renderEntryChild entries
