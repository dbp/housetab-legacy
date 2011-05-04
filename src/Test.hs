{-# LANGUAGE NoMonomorphismRestriction #-}

import            Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, quickCheck)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Bson (cast',val, Val(..))
import            Text.Printf (printf)

import            State

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` arbitrary

instance Arbitrary HouseTabEntry where
  arbitrary = do
    id <- arbitrary
    who <- elements ['A'..'Z']
    what <- arbitrary
    when <- arbitrary
    howmuch <- arbitrary
    whopays <- listOf $ elements ['A'..'Z']
    return (HouseTabEntry id (B8.pack [who]) what when howmuch (B8.pack whopays))

instance Arbitrary Date where
  arbitrary = do
    year <- elements [1900..2100]
    month <- elements [1..12]
    day <- elements [1..31]
    return (Date year month day)
    
instance Arbitrary Person where
  arbitrary = do
    name <- arbitrary
    letter <- elements ['A'..'Z']
    percs <- listOf $ arbitrary
    return (Person name letter percs)

instance Arbitrary HouseTab where
  arbitrary = do
    entries <- listOf $ arbitrary
    people <- listOf $ arbitrary
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
  
    
prop_bson_id :: (Arbitrary a, Val a) => a -> Bool
prop_bson_id v = Just v == cast' (val v)


tests  = [("date.cast'.val/id", quickCheck (prop_bson_id :: Date -> Bool))
         ,("entry.cast'.val/id", quickCheck (prop_bson_id :: HouseTabEntry -> Bool))
         ,("person.cast'.val/id", quickCheck (prop_bson_id :: Person -> Bool))
         ,("housetab.cast'.val/id", quickCheck (prop_bson_id :: HouseTab -> Bool))
         ,("result.cast'.val/id", quickCheck (prop_bson_id :: Result -> Bool))
         ,("account.cast'.val/id", quickCheck (prop_bson_id :: Account -> Bool))

         ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests