module Chapter9 where

import           Data.Bool (bool)
import           Data.Char

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- EnumFromTo
eft :: (Eq a, Enum a) => a -> a -> [a]
eft x y =
  if x == y
    then [x]
    else x : eft (succ x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- Comprehend Thy Lists
mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 10]] -- [1,4,9,16,25,36,49,64,81,100]

q1 :: [Integer]
q1 = [x | x <- mySqr, rem x 2 == 0] -- [4,16,36,64,100] ✅

q2 :: [(Integer, Integer)]
q2 =
  [ (x, y)
  | x <- mySqr
  , y <- mySqr
  , x < 50
  , y > 50 -- [(1,64), (4,81), (9,100)] ❌ much longer!!!
  ]

q3 :: [(Integer, Integer)]
q3 =
  take
    5
    [ (x, y)
    | x <- mySqr
    , y <- mySqr
    , x < 50
    , y > 50 -- [(1,64), (1,81), (1,100), (4,64), (4,81)] ❌
    ]

-- Square Cube
mySqur = [x ^ 2 | x <- [1 .. 5]]

myCube = [x ^ 3 | x <- [1 .. 5]]

ex1 =
  length
    [ (x, y)
    | x <- mySqur
    , y <- myCube
    , x < 50
    , y < 50 -- > 15
    ]

-- Bottom Madness
-- 1) [x^y | x <- [1..5], y <- [2, undefined]] > bottom! ✅
-- 2) take 1 $ [x^y | x <- [1..5], y <- [2, undefined]] > value ✅
-- 3) sum [1, undefined, 3] > bottom! ✅
-- 4) length [1, 2, undefined] > value ✅
-- 5) length $ [1, 2, 3] ++ undefined > bottom! ✅
-- 6) take 1 $ filter even [1, 2, 3, undefined] > value
-- 7) take 1 $ filter even [1, 3, undefined] > value ❌ bottom! (no even...)
-- 8) take 1 $ filter odd [1, 3, undefined] > value ✅
-- 9) take 2 $ filter odd [1, 3, undefined] > value ✅
-- 10) take 3 $ filter odd [1, 3, undefined] > bottom! ✅
-- Is it in normal form?
-- 1) [1, 2, 3, 4, 5] -> NF
-- 2) 1 : 2 : 3 : 4 : _ -> WHNF
-- 3) enumFromTo 1 10 -> neither
-- 4) length [1, 2, 3, 4, 5] -> neither
-- 5) sum (enumFromTo 1 10) -> WHNF
-- 6) ['a'..'m'] ++ ['n'..'z'] -> neither
-- 7) (_, 'b') -> WHNF
-- More Bottoms
-- 1) take 1 $ map (+1) [undefined, 2, 3] -> bottom! ✅
-- 2) take 1 $ map (+1) [1, undefined, 3] -> value ✅
-- 3) take 2 $ map (+1) [1, undefined, 3] -> bottom! ✅
-- 4) will return an array of true for every vowel and false otherwise ✅
itIsMistery :: String -> [Bool]
itIsMistery xs = map (\x -> elem x "aeiou") xs

-- 5)
-- a) map (^2) [1..10] -> [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] ✅
-- b) map minimum [[1..10], [10..20], [20..30]] -> [1, 10, 20] ✅
-- c) map sum [[1..5], [1..5], [1..5]] -> [15, 15, 15] ✅
-- 6)
negateThree :: [Integer] -> [Integer]
negateThree = map (\x -> bool x (-x) (x == 3))

-- Filtering
-- 1)
allMultsOfThree :: [Integer] -> [Integer]
allMultsOfThree = filter ((== 0) . (flip rem 3))

-- 2)
howManyMults :: Int
howManyMults = (length . allMultsOfThree) [1 .. 30]

-- 3)
myFilter :: String -> [String]
myFilter = filter (not . (flip elem ["the", "a", "an"])) . words

-- Zipping exercises
-- 1)
zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

-- 2)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ []          = []
zipWith' _ [] _          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3) done!
-- Data.Char
-- 1) Char -> Bool && Char -> Char
-- 2) filterCaps "HbEfLrLxO" > "HELLO"
filterCaps :: String -> String
filterCaps = filter isUpper

-- 3)
capitalize :: String -> String
capitalize (h:t) = toUpper h : t

-- 4)
caps :: String -> String
caps []    = []
caps (h:t) = toUpper h : caps t

-- 5)
capHead :: String -> Char
capHead = toUpper . head

-- 6) done!!
-- Writing my own standard functions
-- 1)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem _ []      = False
myElem x' (x:xs) = x == x' || myElem x' xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

-- 4)
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5)
squish :: [[a]] -> [a]
squish []      = []
squish (xs:ys) = xs ++ squish ys

-- 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish (map f xs)

-- 7)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[])   = x
myMaximumBy f (x:y:[]) = bool y x (f x y == GT)
myMaximumBy f (x:y:xs) = myMaximumBy f (bool y x (f x y == GT) : xs)

-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[])   = x
myMinimumBy f (x:y:[]) = bool y x (f x y == LT)
myMinimumBy f (x:y:xs) = myMinimumBy f (bool y x (f x y == LT) : xs)

-- 10)
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
