module Cipher where

import Data.Bool (bool)
import Data.Char (chr, ord, isUpper)

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (ord c - a + n) 26 + a
  where a = ord $ bool 'a' 'A' (isUpper c)

toCaesar :: String -> String
toCaesar = map $ cipher 3

unCaesar :: String -> String
unCaesar = map $ cipher (-3)

shift :: Char -> Int
shift x = ord x - (ord $ bool 'a' 'A' (isUpper x))

toVigenere :: String -> String -> String
toVigenere secret = zipWith (cipher . shift) (cycle secret) . concat . words

fromVigenere :: String -> String -> String
fromVigenere secret = zipWith (cipher . negate . shift) (cycle secret) . concat . words
