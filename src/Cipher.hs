module Cipher where

import Data.Char

cipher :: Int -> Char -> Char
cipher n c = (chr . (+a) . (flip mod 26) . (+n) . (subtract a) . ord) c
  where a = ord (if isUpper c then 'A' else 'a')

toCaesar :: String -> String
toCaesar = map $ cipher 3

unCaesar :: String -> String
unCaesar = map $ cipher (-3)
