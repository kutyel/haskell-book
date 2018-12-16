module Cipher where

import Data.Char

encrypt :: Int -> Char -> Char
encrypt n = chr . (+a) . (flip mod 26) . (+n) . (subtract a) . ord
  where a = ord 'A'

toCaesar :: String -> String
toCaesar = map $ encrypt 3

unCaesar :: String -> String
unCaesar = map $ encrypt (-3)
