module Chapter7 where

-- Grab bag

-- 1) all of them are equivalent!
mTh :: Integer -> Integer -> Integer -> Integer
mTh = \x -> \y -> \z -> x * y * z

-- 2) Integer -> Integer -> Integer

-- 3
-- a)
addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1
-- b)
addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5
-- c)
mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x

-- Variety Pack

-- 1)
-- a)
k :: (a, b) -> a
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)
-- b) k2 :: String != k2 != k3
-- c) k1 and k3!

-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- Case Practice

-- 1)
functionC :: Ord a => a -> a -> a
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2)
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case even n of
    True -> f n
    False -> n
    where f = (+ 2)

-- 3)
nums :: (Ord a, Num a, Num b) => a -> b
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
