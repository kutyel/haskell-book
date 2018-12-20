module Chapter10 where

import Data.Bool (bool)

-- foldr f z [1, 2, 3]
-- 1 `f` (foldr f z [2, 3])
-- 1 `f` (2 `f` (foldr f z [3]))
-- 1 `f` (2 `f` (3 `f` (foldr f z [])))
-- 1 `f` (2 `f` (3 `f` z))

-- foldl (flip const) 0 [1..5]
-- f ~ (flip const); z ~ 0
-- (((((0 `f` 1) `f` 2) `f` 3) `f` 4) `f` 5)
-- ((((1 `f` 2) `f` 3) `f` 4) `f` 5)
-- (((2 `f` 3) `f` 4) `f` 5)
-- ((3 `f` 4) `f` 5)
-- (4 `f` 5)
-- 5

-- Understanding Folds

-- 1)
ex1 = foldr (*) 1 [1..5] == foldl (*) 1 [1..5] -- b) && c)

-- 2) foldl (flip (*)) 1 [1..3]
-- f ~ (flip (*)); z ~ 1
-- (((1 `f` 1) `f` 2) `f` 3)
-- (((1 * 1) * 2) * 3)
-- ((1 * 2) * 3)
-- (2 * 3)
-- 6

-- 3) one difference between `foldr` and `foldl` is:
-- c) `foldr`, but not `foldl`, associates to the right

-- 4) catamorphisms are usually used to...
-- a) ...reduce structure!

-- 5)
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) True [False, True] -- always returns `True`
e = foldl (flip ((++) . show)) "" [1..5]
f = foldr (flip const) 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]

-- Scan Exercises

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN n = fibs !! n

-- 1)
fibs20 :: Integer
fibs20 = fibsN 20

-- 2)
fibsLessThan100 :: [Integer]
fibsLessThan100 = takeWhile (<100) fibs

-- 3) 🤯
fact :: [Integer]
fact = scanl (*) 1 [1..]

factN :: Int -> Integer
factN n = fact !! n

-- Warm-up and review

stops :: String
stops  = "pbtdkg"
vowels :: String
vowels = "aeiou"

nouns :: [String]
nouns = ["cat", "dog", "tiger", "lynx", "mice"]
verbs :: [String]
verbs = ["eats", "hunts", "plays with"]

ex :: [a] -> [b] -> [(a, b, a)]
ex xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

-- 1)
a1 = ex stops vowels
b1 = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']
c1 = ex nouns verbs

-- 2)
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
-- takes a sentence and returns the ratio between chars and num of words

-- 3)
seekritFunc' :: String -> Double
seekritFunc' x = dd / dv
  where dd = (fromIntegral . sum . (map length) . words) x
        dv = (fromIntegral . length . words) x

-- Rewriting functions using folds

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||). f) False

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

-- 4)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5)
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> bool (x:xs) xs (f x)) []

-- 7)
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> bool b a (f a b == GT)) x xs

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> bool b a (f a b == LT)) x xs
