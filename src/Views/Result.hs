{-# LANGUAGE OverloadedStrings #-}

module Views.Result where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe,listToMaybe)
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Data.List (intercalate, sortBy)
import            Data.List.Split (splitEvery)

import            Control.Monad.Trans (lift,liftIO)

import            Data.Time.Calendar
import            Data.Time.LocalTime
import            Data.Time.Clock

import            Application

import            Models.Result
import            Models.Entry
import            Models.Person

import            Views.Site
import            Views.Person

renderPersonResult :: Monad m => Day -> (String,Person,Spent,Owes) -> Splice m
renderPersonResult today (klasses,person,spent,owes) = do
  runChildrenWith 
    ([("personSpent",        textSplice $ T.pack $ moneyShow spent)
     ,("personOwes",         textSplice $ T.pack $ moneyShow (negate owes))
     ,("personCurrentShare", textSplice $ T.pack $ maybe "0" (show.snd) $ personShareAsOf today person)
     ,("personClasses",      textSplice $ T.pack klasses)
     ] ++ (renderPerson person))
      where personShareAsOf day (Person _ _ _ shares) = listToMaybe $ reverse $ takeWhile ((< day).fst) $ sortBy (\a b -> compare (fst a) (fst b)) $ map (\(Share d v) -> (dateToDay d,v)) shares 
                       
renderResult :: Result -> Splice Application
renderResult (Result people date) = do now <- lift $ liftIO $ getCurrentTime
                                       zone <- lift $ liftIO $ getCurrentTimeZone
                                       mapSplices 
                                        (renderPersonResult $ localDay $ utcToLocalTime zone now) 
                                        (addClasses people)
  where addClasses ps = concat $ map (addLast "last") $ addLastAll "bottom" $ pad $ splitEvery 6 $ addClassSpot ps
        -- pad adds in extra spaces to the last row, if needed
        pad l = init l ++ [last l ++ (take (6 - length (last l)) $ repeat ("",emptyPerson,0,0))] 
        addClassSpot = map (\(a,b,c) -> ("",a,b,c))
        addC :: String -> (String,Person,Spent,Owes) -> (String,Person,Spent,Owes)
        addC s l       = (\(a,b,c,d) -> (a ++ " " ++ s,b,c,d)) l
        addLast :: String -> [(String,Person,Spent,Owes)] -> [(String,Person,Spent,Owes)]
        addLast s l    = init l ++ [addC s (last l)]
        addLastAll :: String -> [[(String,Person,Spent,Owes)]] -> [[(String,Person,Spent,Owes)]]
        addLastAll s l = init l ++ [map (addC s) (last l)]