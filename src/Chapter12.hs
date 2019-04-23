module Chapter12 where

import Data.Char (toLower)
import Data.List (partition)

-- Determine the kinds
-- 1) id :: a -> a ? * -> *
-- 2) r :: a -> f a ? a = *, f = * -> *
-- String processing
-- 1
notThe :: String -> Maybe String
notThe s =
  if s == "the"
    then Nothing
    else Just s

replaceThe :: String -> String
replaceThe = unwords . (map replace) . words
  where
    replace x =
      case notThe x of
        Nothing -> "a"
        Just x -> x

-- 2
isVowel :: Char -> Bool
isVowel = flip elem "aeiou" . toLower

startsWithVowel :: String -> Bool
startsWithVowel = isVowel . head

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0
  where
    go (_:[]) count = count
    go (x:y:xs) count =
      go
        (y : xs)
        (if x == "the" && startsWithVowel y
           then count + 1
           else count)

-- 3
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

-- validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | length vs > length cs = Nothing
  | otherwise = Just (Word' s)
  where
    (vs, cs) = partition isVowel s

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n =
  case n of
    Zero -> 0
    Succ n -> 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (toNat x)
  where
    toNat n =
      case n of
        0 -> Zero
        n -> Succ (toNat (n - 1))
