{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}

import            Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, listOf1, choose, quickCheck)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Bson (cast',val, Val(..), ObjectId(..))
import            Text.Printf (printf)
import            Data.List (nub, sort, sortBy, delete)
import            Data.Ord (comparing)
import qualified  Data.Text.Encoding as TE
import            Data.Maybe (fromJust)

import            Models.Entry
import            Models.Person
import            Models.Result
import            Models.Site (emptyObjectId)
import            Views.Site (categoryList)
import            Lib

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` arbitrary

instance Arbitrary ObjectId where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   return (Oid a b)

housetabentry_arbitrary_restricted people start = do
      id' <- arbitrary
      who <- elements people
      what <- arbitrary
      when <- date_arbitrary_restricted start
      category <- elements (map TE.encodeUtf8 categoryList)
      howmuch <- choose (0,1000000)
      whopays <- listOf1 $ elements people
      return (HouseTabEntry (Just id') emptyObjectId who what category when howmuch (nub whopays))


instance Arbitrary HouseTabEntry where
  arbitrary = do
    people <- listOf1 arbitrary
    housetabentry_arbitrary_restricted people (Date 1900 1 1)
  shrink (HouseTabEntry id' htid who what category when howmuch whopays) =
    if length whopays > 1 then map (\w -> (HouseTabEntry id' htid who what category when howmuch  (delete w whopays))) whopays else []

date_arbitrary_restricted (Date year month day) = do
   y <- elements [year..2100]
   m <- if y == year then elements [month..12] else elements [1..12]
   d <- if y == year && m == month then elements [day..31] else elements [1..31]
   return (Date y m d)

instance Arbitrary Date where
  arbitrary = date_arbitrary_restricted (Date 1900 1 1)
    
instance Arbitrary Share where
  arbitrary = do
    d <- arbitrary
    v <- choose (0,1000)
    return (Share d v)

instance Arbitrary Person where
  arbitrary = do
    name <- arbitrary
    id' <- arbitrary
    shares <- listOf1 $ arbitrary
    return (Person (Just id') emptyObjectId name (reverse $ sort $ nub shares))
  shrink (Person id' htid name shares) =
    if length shares > 1 then map (\s -> (Person id' htid name (delete s shares))) shares else []
    
earliest_allin people = (head $ reverse $ sort $ map (sDate.head) $ map (reverse.pShares) people)

instance Arbitrary Result where
  arbitrary = do
    people <- fmap nub $ listOf $ arbitrary
    date <- arbitrary
    return (Result people date)
  
instance Arbitrary Purchase where
  arbitrary = do
    people <- fmap nub $ listOf1 $ arbitrary
    entry <- housetabentry_arbitrary_restricted (map (fromJust.pId) people) (earliest_allin people)
    return (head $ purchasify people [entry])

data EntriesPeople = EP [Person] [HouseTabEntry] deriving (Show,Eq)

instance Arbitrary EntriesPeople where
  arbitrary = do
    people  <- fmap nub $ listOf1 $ arbitrary
    entries <- listOf1 $ housetabentry_arbitrary_restricted (map (fromJust.pId) people) (earliest_allin people)
    return (EP people entries)
  shrink (EP people entries) = 
    map (\e -> EP people (delete e entries)) entries
    

almostEq a b = (abs $ a - b) < 0.001
    
prop_bson_id :: (Arbitrary a, Val a) => a -> Bool
prop_bson_id v = Just v == cast' (val v)

prop_run_owed_eq_zero :: EntriesPeople -> Bool
prop_run_owed_eq_zero (EP ps entries) = 0 `almostEq` (sum $ map (\(a,b,c) -> c) result)
  where result = people $ run ps entries
  
prop_run_spent_eq_total :: EntriesPeople -> Bool
prop_run_spent_eq_total (EP ps entries) = sum (map eHowmuch entries) `almostEq` (sum $ map (\(a,b,c) -> b) result)
  where result = people $ run ps entries

prop_dothesplit_sum :: Purchase -> Bool
prop_dothesplit_sum p = (ammount p) `almostEq` (sum $ map snd ls)
  where ls = doTheSplit p 
  

tests  = [("date.cast'.val/id", quickCheck (prop_bson_id :: Date -> Bool))
         ,("entry.cast'.val/id", quickCheck (prop_bson_id :: HouseTabEntry -> Bool))
         ,("person.cast'.val/id", quickCheck (prop_bson_id :: Person -> Bool))
         ,("result.cast'.val/id", quickCheck (prop_bson_id :: Result -> Bool))
         ,("lib.run.owed.sum/zero", quickCheck prop_run_owed_eq_zero)
         ,("lib.run.spent.sum/total", quickCheck prop_run_spent_eq_total)
         ,("dothesplit.sum/ammount", quickCheck prop_dothesplit_sum)
         ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests