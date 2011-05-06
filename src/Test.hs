{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

import            Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, listOf1, choose, quickCheck)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Bson (cast',val, Val(..))
import            Text.Printf (printf)
import            Data.List (nub, sort)

import            State
import            Lib

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` arbitrary


housetabentry_arbitrary_restricted people = do
      id <- arbitrary
      who <- elements people
      what <- arbitrary
      when <- arbitrary
      howmuch <- choose (0,1000000)
      whopays <- listOf1 $ elements people
      return (HouseTabEntry id (B8.pack [who]) what when howmuch (B8.pack (nub whopays)))


instance Arbitrary HouseTabEntry where
  arbitrary = housetabentry_arbitrary_restricted ['A'..'Z']

instance Arbitrary Date where
  arbitrary = do
    year <- elements [1900..2100]
    month <- elements [1..12]
    day <- elements [1..31]
    return (Date year month day)
    
instance Arbitrary Percent where
  arbitrary = do
    d <- arbitrary
    p <- choose (0,1)
    return (Percent d p)

instance Arbitrary Person where
  arbitrary = do
    name <- arbitrary
    letter <- elements ['A'..'Z']
    percs <- listOf1 $ arbitrary
    return (Person name letter (reverse $ sort percs))

instance Arbitrary HouseTab where
  arbitrary = do
    people <- listOf1 $ arbitrary
    entries <- listOf $ housetabentry_arbitrary_restricted (map letter people)
    return (HouseTab entries people)

instance Arbitrary Result where
  arbitrary = do
    people <- listOf $ arbitrary
    date <- arbitrary
    return (Result people date)

instance Arbitrary Account where
  arbitrary = do
    id <- arbitrary
    name <- arbitrary
    emails <- listOf $ arbitrary
    housetab <- arbitrary
    current <- arbitrary
    reset <- arbitrary
    activate <- arbitrary
    return (Account id name emails housetab current reset activate)
  
instance Arbitrary Purchase where
  arbitrary = do
    people <- listOf1 $ arbitrary
    entry <- housetabentry_arbitrary_restricted (map letter people)
    return (head $ purchasify people [entry])


almostEq a b = (abs $ a - b) < 0.001
    
prop_bson_id :: (Arbitrary a, Val a) => a -> Bool
prop_bson_id v = Just v == cast' (val v)

prop_run_owed_eq_zero :: HouseTab -> Bool
prop_run_owed_eq_zero ht = 0 `almostEq` (sum $ map (\(a,b,c) -> c) result)
  where result = people $ run ps entries
        ps = houseTabPeople ht
        entries = houseTabEntries ht
  
prop_run_spent_eq_total :: HouseTab -> Bool
prop_run_spent_eq_total ht = sum (map ehowmuch entries) `almostEq` (sum $ map (\(a,b,c) -> b) result)
  where result = people $ run ps entries
        ps = houseTabPeople ht
        entries = houseTabEntries ht

prop_dothesplit_sum :: Purchase -> Bool
prop_dothesplit_sum p = (ammount p) `almostEq` (sum $ map snd ls)
  where ls = doTheSplit p 
  

tests  = [("date.cast'.val/id", quickCheck (prop_bson_id :: Date -> Bool))
         ,("entry.cast'.val/id", quickCheck (prop_bson_id :: HouseTabEntry -> Bool))
         ,("person.cast'.val/id", quickCheck (prop_bson_id :: Person -> Bool))
         {-,("housetab.cast'.val/id", quickCheck (prop_bson_id :: HouseTab -> Bool))
         ,("result.cast'.val/id", quickCheck (prop_bson_id :: Result -> Bool))
         ,("account.cast'.val/id", quickCheck (prop_bson_id :: Account -> Bool))-}
         ,("lib.run.owed.sum/zero", quickCheck prop_run_owed_eq_zero)
         ,("lib.run.spent.sum/total", quickCheck prop_run_spent_eq_total)
         ,("dothesplit.sum/ammount", quickCheck prop_dothesplit_sum)
         ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests