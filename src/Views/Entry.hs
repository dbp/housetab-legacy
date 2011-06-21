{-# LANGUAGE OverloadedStrings #-}

module Views.Entry where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import qualified  Data.Map as M

import            Data.Maybe (fromMaybe, maybeToList)
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.ByteString as BS
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Models.Entry

renderEntryChild :: Monad m => HouseTabEntry -> Splice m
renderEntryChild entry = runChildrenWith (renderEntry entry)

renderEntry :: Monad m => HouseTabEntry -> [(T.Text, Splice m)]
renderEntry (HouseTabEntry uid htid who what category when howmuch whopays) =
  [("entryFor",    mapSplices runChildrenWithText (map ((:[]) . ((,) "value") . TE.decodeUtf8 . objid2bs) whopays))] ++
  (map ((\(a,b) -> (a, textSplice b)))
   [("index",         TE.decodeUtf8 (maybe "" objid2bs uid))
   ,("htid",          TE.decodeUtf8 $ objid2bs htid)
   ,("entryBy",       TE.decodeUtf8 $ objid2bs who)
   ,("entryWhat",     TE.decodeUtf8 what)
   ,("entryCategory", TE.decodeUtf8 category)
   ,("entryDate",     T.pack $ show when)
   ,("entryAmount",   T.pack $ show howmuch)
   ])                       

renderEntries :: Monad m => [HouseTabEntry] -> Splice m
renderEntries entries = mapSplices renderEntryChild entries

categories :: Monad m => Splice m
categories = mapSplices runChildrenWithText (map ((:[]) . ((,) "cat")) categoryList) 


categoryList :: [T.Text]
categoryList = ["alcohol"
               ,"cash"
               ,"entertainment"
               ,"furnishings"
               ,"groceries"
               ,"household"
               ,"misc"
               ,"food"
               ,"rent"
               ,"toiletries"
               ,"utilities"
               ]

catImages = M.fromList [("alcohol", "/img/alcohol.png")
                       ,("cash", "/img/cash.png")
                       ,("entertainment", "/img/entertainment.png")
                       ,("furnishings", "/img/furnishings.png")
                       ,("groceries", "/img/groceries.png")
                       ,("household", "/img/household.png")
                       ,("misc", "/img/misc.png")
                       ,("food", "/img/pan.png")
                       ,("rent", "/img/rent.png")
                       ,("toiletries", "/img/toiletries.png")
                       ,("utilities", "/img/utilities.png")
                       ]

catNames = M.fromList [("alcohol", "Alcohol")
                      ,("cash", "Cash")
                      ,("entertainment", "Entertainment")
                      ,("furnishings", "Furnishings")
                      ,("groceries", "Groceries")
                      ,("household", "Household")
                      ,("misc", "Miscellanea")
                      ,("food", "Food")
                      ,("rent", "Rent")
                      ,("toiletries", "Toiletries")
                      ,("utilities", "Utilities")
                      ]

categoryImage :: Monad m => Splice m
categoryImage = do node <- getParamNode
                   case X.getAttribute "cat" node of
                      Nothing -> return [] -- no id, so no name
                      Just id' -> return $ maybeToList $ fmap X.TextNode $ M.lookup id' catImages
                   

categoryName :: Monad m => Splice m
categoryName = do node <- getParamNode
                  case X.getAttribute "cat" node of
                     Nothing -> return [] -- no id, so no name
                     Just id' -> return $ maybeToList $ fmap X.TextNode $ M.lookup id' catNames