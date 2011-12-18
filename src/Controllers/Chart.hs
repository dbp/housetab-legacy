{-# LANGUAGE OverloadedStrings #-}

module Controllers.Chart where

import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Snap.Extension.DB.MongoDB hiding (group, sort, Array)
import qualified  Data.Map as M
import            Control.Monad
import            Control.Applicative
import            Control.Monad.Trans
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T

import            Data.Aeson
import qualified  Data.Vector as V

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust, isNothing, mapMaybe)
import qualified  Data.Bson as B
import            Data.List (sortBy, sort, group, groupBy)
import            Data.Ord (comparing)
import            Data.List.Split
import            System.Random


import            Text.Digestive.Types
import            Text.Digestive.Snap.Heist
import            Text.Digestive.Validate
import            Text.Digestive.Transform
import            Data.Text (Text)
import            Text.Templating.Heist
import qualified  Text.XmlHtml as X

import            Data.Time.Calendar
import            Data.Time.LocalTime
import            Data.Time.Clock

import            Application
import            Lib
import            Utils
import            Views.Site
import            Views.Result
import            Views.Person
import            Models.Person
import            Models.Site
import            Models.Account
import            Models.Entry

showCharts :: User -> Application ()
showCharts user = do entries <- getHouseTabEntriesAll (authUser user)
                     people <- getHouseTabPeople (authUser user)
                     let categories = calculateCategoryTotals entries
                     (heistLocal $ (bindSplices
                      ([ ("result",           (renderResult  $ currentResult user))
                       , ("totalSpent",       textSplice $ T.pack $ moneyShow $ getTotalSpent user)
                       , ("category-data",       categoryData categories)
                       , ("entriesPage",      textSplice $ "1")
                       , ("for-value",        textSplice $ T.intercalate "," $ map (TE.decodeUtf8 . objid2bs) $ mapMaybe pId people)
                       ]))) $ renderHT "charts"

calculateCategoryTotals :: [HouseTabEntry] -> [(BS.ByteString,Double)]
calculateCategoryTotals entries = 
    normalize $ foldr totals [] $ groupBy cmp $ sortBy (comparing fst) $ map (\e -> (eCategory e,eHowmuch e)) entries
  where cmp e1 e2 = (fst e1) == (fst e2)
        totals [] b = b
        totals ((cat,n):xs) b = (cat,sum (n:(map snd xs))):b
        normalize ls = let total = sum (map snd ls) in map (\(c,n) -> (c,n/total * 100)) ls
        
categoryData :: [(BS.ByteString,Double)] -> Splice Application
categoryData cs = return [X.Element "script" [("type","text/javascript")] [X.TextNode $ T.pack ("var cat_data = " ++ (show $ map snd cs) ++ ";"), X.TextNode $ T.pack ("var cat_legend = " ++ (show $ map ((BS.append "%% ").fst) cs) ++ ";")]]
