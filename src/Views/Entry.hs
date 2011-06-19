{-# LANGUAGE OverloadedStrings #-}

module Views.Entry where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe)
import qualified  Data.ByteString.Char8 as B8
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Models.Entry

renderEntry :: Monad m => HouseTabEntry -> Splice m
renderEntry (HouseTabEntry uid htid who what when howmuch whopays) = do
  runChildrenWithText [("index",       TE.decodeUtf8 (maybe "" objid2bs uid))
                      ,("htid",        TE.decodeUtf8 $ objid2bs htid)
                      ,("entryBy",     TE.decodeUtf8 $ objid2bs who)
                      ,("entryWhat",   TE.decodeUtf8 what)
                      ,("entryDate",   T.pack $ show when)
                      ,("entryAmount", T.pack $ show howmuch)
                      ,("entryFor",    TE.decodeUtf8 (B8.intercalate "," (map objid2bs whopays)))
                      ]
                       
renderEntries :: Monad m => [HouseTabEntry] -> Splice m
renderEntries entries = mapSplices renderEntry entries
