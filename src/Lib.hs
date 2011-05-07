module Lib where

import Data.List (groupBy, sort, notElem)
import State (HouseTabEntry(..), Result(..),Date(..),Percent(..),Person(..))

import Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, listOf1, choose, quickCheck)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as B8

data Purchase = Purchase { purchaser :: Person
                         , date :: Date
                         , ammount :: Double
                         , payers :: [Person]}
              deriving (Show, Read, Eq)

instance Ord Purchase where
    compare (Purchase p1 d1 _ _) (Purchase p2 d2 _ _) =
        if compare d1 d2 == EQ then compare p1 p2 else compare d1 d2

getPercentage :: Person -> Date -> Double
getPercentage person date = fn (percs person)
    where
      fn ((Percent d p):[]) = p
      fn ((Percent d p):(Percent d' p'):xs) = if d' > date
                    then p
                    else fn $ (Percent d' p'):xs
      fn [] = 0 -- if they have nothing assigned, assume no share.

getPerson :: [Person] -> Char -> Person
getPerson ps l = head $ filter (\p -> (letter p) == l) ps

purchasify :: [Person] -> [HouseTabEntry] -> [Purchase]
purchasify people ((HouseTabEntry who _ when howmuch whopays):xs) = 
    (Purchase (getPerson people (B8.head who))
              when
              howmuch 
              (map (getPerson people) (B8.unpack whopays))) : (purchasify people xs)
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