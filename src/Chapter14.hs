module Chapter14 where

import           Chapter11 (capitalizeWord)
import           Data.List (sort)

half :: Fractional a => a -> a
half = (/ 2)

square :: Num a => a -> a
square x = x * x

squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, t)       = (Just y, x >= y)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

associative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

commutative :: (Eq a) => (a -> a -> a) -> a -> a -> Bool
commutative f x y = x `f` y == y `f` x

prop_half :: (Eq a, Fractional a) => a -> Bool
prop_half x = half x * 2 == x

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

prop_reverseTwice :: Eq a => [a] -> Bool
prop_reverseTwice x = (reverse . reverse) x == id x

prop_applyOperator :: Eq a => a -> Bool
prop_applyOperator x = id $ x == id x

prop_composition :: Eq a => a -> Bool
prop_composition x = (id . id) x == id (id x)

prop_roundTrip :: (Eq a, Read a, Show a) => a -> Bool
prop_roundTrip x = (read . show) x == x

prop_foldrPlusPlus :: Eq a => [a] -> [a] -> Bool
prop_foldrPlusPlus x y = foldr (:) x y == (++) x y

prop_foldrConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrConcat x = foldr (++) [] x == concat x

prop_takeLength :: Int -> [a] -> Bool
prop_takeLength n xs = length (take n xs) == n

prop_idemCapitalize :: String -> Bool
prop_idemCapitalize x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

prop_idemSort :: Ord a => [a] -> Bool
prop_idemSort x = (sort x == twice sort x) && (sort x == fourTimes sort x)
