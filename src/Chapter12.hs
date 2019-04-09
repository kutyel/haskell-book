module Chapter12 where

-- Determine the kinds

-- 1) id :: a -> a ? * -> *
-- 2) r :: a -> f a ? a = *, f = * -> *

-- String processing

-- 1
notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise  = Just s

replaceThe :: String -> String
replaceThe = unwords . (map replace) . words
  where replace x = case notThe x of
          Nothing -> "a"
          Just x  -> x

-- 2

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

-- 3

countVowels :: String -> Integer
countVowels = undefined
