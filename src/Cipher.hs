module Cipher where

import Data.Char

encrypt :: Int -> Char -> Char
encrypt n c = (chr . (+a) . (flip mod 26) . (+n) . (subtract a) . ord) c
  where a = if isUpper c then ord 'A' else ord 'a'

toCaesar :: String -> String
toCaesar = map $ encrypt 3

unCaesar :: String -> String
unCaesar = map $ encrypt (-3)
