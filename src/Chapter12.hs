module Chapter12 where

import Data.Char (toLower)

-- Determine the kinds

-- 1) id :: a -> a ? * -> *
-- 2) r :: a -> f a ? a = *, f = * -> *

-- String processing

-- 1
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe = unwords . (map replace) . words
  where replace x = case notThe x of
          Nothing -> "a"
          Just x  -> x

-- 2

isVowel :: Char -> Bool
isVowel = flip elem "aeiou" . toLower

startsWithVowel :: String -> Bool
startsWithVowel = isVowel . head

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0
  where go (_:[]) count = count
        go (x:y:xs) count = go (y:xs) (if x == "the" && startsWithVowel y then count + 1 else count)

-- 3

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
