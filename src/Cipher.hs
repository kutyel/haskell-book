module Cipher where

import Data.Bool (bool)
import Data.Char

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (ord c - a + n) 26 + a
  where a = ord $ bool 'a' 'A' (isUpper c)

toCaesar :: String -> String
toCaesar = map $ cipher 3

unCaesar :: String -> String
unCaesar = map $ cipher (-3)
