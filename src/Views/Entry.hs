{-# LANGUAGE OverloadedStrings #-}

module Views.Entry where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import qualified  Data.Map as M
import            Data.List (intercalate)

import            Data.Maybe (fromMaybe, maybeToList)
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.ByteString as BS
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Models.Entry
import            Models.Person

import            Views.Site

renderEntryChild :: Monad m => [Person] -> HouseTabEntry -> Splice m
renderEntryChild people entry = runChildrenWith (renderEntry people entry)

renderEntry :: Monad m => [Person] -> HouseTabEntry -> [(T.Text, Splice m)]
renderEntry people (HouseTabEntry uid htid who what category when howmuch whopays) =
  [("entryFor",         forSplice whopays)] ++
  (map ((\(a,b) -> (a, textSplice b)))
   [("index",           TE.decodeUtf8 (maybe "" objid2bs uid))
   ,("htid",            TE.decodeUtf8 $ objid2bs htid)
   ,("entryBy",         TE.decodeUtf8 $ objid2bs who)
   ,("entryWhat",       TE.decodeUtf8 what)
   ,("entryCategory",   TE.decodeUtf8 category)
   ,("entryDate",       T.pack $ show when)
   ,("entryAmount",     T.pack $ moneyShow howmuch)
   ,("entryForSummary", T.pack $ (showPeople people whopays))
   ])


renderEntries :: Monad m => [Person] -> [HouseTabEntry] -> Splice m
renderEntries people entries = mapSplices (renderEntryChild people) entries