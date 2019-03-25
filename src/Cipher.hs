module Cipher where

import Data.Bool (bool)
import Data.Char
import Data.List.Index (indexed)

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (ord c - a + n) 26 + a
  where a = ord $ bool 'a' 'A' (isUpper c)

toCaesar :: String -> String
toCaesar = map $ cipher 3

unCaesar :: String -> String
unCaesar = map $ cipher (-3)

getShift :: Char -> Int
getShift x = ord x - a
  where a = ord $ bool 'a' 'A' (isUpper x)

toVigenere :: String -> String -> String
toVigenere key = map (\(i, x) -> cipher (getShift (key !! i)) x) . indexed
