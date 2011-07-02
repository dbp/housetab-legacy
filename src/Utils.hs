module Utils where

import qualified  Data.ByteString.Char8 as B8
import Data.Maybe (listToMaybe)

eitherToMaybe = either (const Nothing) Just

strMaybe s = case s of
              "" -> Nothing
              x -> Just (B8.pack x)

findReplace fn val []     = val:[]
findReplace fn val (x:xs) = if fn x then val:xs else x : (findReplace fn val xs)

-- stolen from cgi:
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
