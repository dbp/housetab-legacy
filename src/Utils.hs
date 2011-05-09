module Utils where

import Data.List (null, elemIndex)
import Test.QuickCheck

findReplace fn val []     = val:[]
findReplace fn val (x:xs) = if fn x then val:xs else x : (findReplace fn val xs)

prop_findreplace_first :: [Int] -> Bool
prop_findreplace_first l = head (findReplace (const True) 1 l) == 1
prop_findreplace_last :: [Int] -> Bool
prop_findreplace_last l = 2 `elem` (findReplace (== 1) 2 newL)
  where newL = l ++ [1]
prop_findreplace_index :: [Int] -> Bool
prop_findreplace_index l = if null l then True else elemIndex fl newL >= elemIndex 2 (findReplace (== fl) 2 newL)
  where fl = head l
        newL = rotate l
        rotate l = (drop half l) ++ (take half l)
        half = (length l `div` 2)
        
test = sequence (map quickCheck [prop_findreplace_first, prop_findreplace_last, prop_findreplace_index])