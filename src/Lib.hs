module Lib where

import Data.List (groupBy, sort, notElem)
import Models.Entry (HouseTabEntry(..),Date(..))
import Models.Result (Result(..)) 
import Models.Person (Share(..),Person(..))
import qualified  Data.ByteString as BS
import qualified  Data.Bson as B

import Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, listOf1, choose, quickCheck)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as B8

data Purchase = Purchase { purchaser :: Person
                         , date :: Date
                         , ammount :: Double
                         , payers :: [Person]}
              deriving (Show, Eq)

instance Ord Purchase where
    compare (Purchase p1 d1 _ _) (Purchase p2 d2 _ _) =
        if compare d1 d2 == EQ then compare p1 p2 else compare d1 d2

getPercentage :: Person -> Date -> Double
getPercentage person date = fn (pShares person)
    where
      fn ((Share d p):[]) = p
      fn ((Share d p):(Share d' p'):xs) = if d' > date
                    then p
                    else fn $ (Share d' p'):xs
      fn [] = 1 -- if they have nothing assigned, assume no share.

getPerson :: [Person] -> B.ObjectId -> Person
getPerson ps id' = head $ filter (\p -> (pId p) == Just id') ps

purchasify :: [Person] -> [HouseTabEntry] -> [Purchase]
purchasify people ((HouseTabEntry _ _ who _ _ when howmuch whopays):xs) = 
    (Purchase (getPerson people who)
              when
              howmuch 
              (map (getPerson people) whopays)) : (purchasify people xs)
purchasify _ [] = []

doTheSplit :: Purchase -> [(Person, Double)]
doTheSplit purchase = map (\p -> (p, (ammount purchase) * (gp p) / (if unity == 0 then 1 else unity))) (payers purchase)
    where unity = sum (map gp (payers purchase))
          gp p = getPercentage p (date purchase)

concatJoin :: [[(Person, Double)]] -> [(Person, Double)]
concatJoin pds = map (\grp -> ((fst (head grp)), sum (map snd grp))) $ 
                 groupBy (\a b -> (fst a) == (fst b)) $ sort $ concat pds

spent :: [Purchase] -> [(Person, Double)]
spent purchases = concatJoin $ [map (\p -> (purchaser p, ammount p)) purchases]

processPurchases :: [Purchase] -> Result
processPurchases [] = Result [] (Date 1000 1 1)
processPurchases purchases = Result (map (pickSpent purchases) $ concatJoin $ map doTheSplit purchases) (date (head (reverse (sort purchases))))

pickSpent purchases (p, a) = (p, spent {- - paybacks-}, a - spent)
    where count ps = sum $ map ammount $ ps
          spent = count $ filter (\pur -> purchaser pur == p) purchases
          -- this is because the total spent looks strange when you don't factor these in.
          -- a payback is classified by one person directly paying another (ie, the first
          -- is the spender, the receiver is only splitter)
          paybacks = count $ filter (\pur -> (length (payers pur) == 1) && (head (payers pur) == p) && (purchaser pur /= p)) purchases
              

addMissing :: [Person] -> Result -> Result
addMissing house (Result personResult date) = Result (personResult ++ (map (\p -> (p,0,0)) missing)) date
    where missing = filter (\p -> notElem p (map (\(p,_,_) -> p) personResult)) house

run :: [Person] -> [HouseTabEntry] -> Result
run house s = addMissing house $ processPurchases $ purchasify house s