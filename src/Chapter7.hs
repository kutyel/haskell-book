module Chapter7 where

-- Grab bag
-- 1) all of them are equivalent!
mTh :: Integer -> Integer -> Integer -> Integer
mTh = \x -> \y -> \z -> x * y * z

-- 2) Integer -> Integer -> Integer
-- 3
-- a)
addOneIfOdd :: Integral a => a -> a
addOneIfOdd n =
  case odd n of
    True  -> f n
    False -> n
  where
    f = \n -> n + 1

-- b)
addFive :: Integer -> Integer -> Integer
addFive =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

-- c)
mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x

-- Variety Pack
-- 1)
-- a)
k :: (a, b) -> a
k (x, y) = x

k1 = k ((4 - 1), 10)

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
    True  -> x
    False -> y

-- 2)
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case even n of
    True  -> f n
    False -> n
  where
    f = (+ 2)

-- 3)
nums :: (Ord a, Num a, Num b) => a -> b
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy
dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

-- 1) 1 ✅
-- 2) 11 ✅
-- 3) 22 ✅
-- 4) 21 ✅
-- 5) 12 ✅
-- 6) 11 ✅
-- 7) 12 ❌ 21!
-- 8) 21 ✅
-- 9) 22 ✅
-- 10) 13 ❌ 31!
-- 11) 23 ✅
-- Guard Duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  | y >= 0.59 = 'E'
  | y < 0.59 = 'F'
  where
    y = x / 100

-- 1) always returns 'F' (| otherwise = 'F')
-- 2) does not work correctly (order matters!)
-- 3)
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- b) True when xs is a palindrome!
-- 4) Lists!
-- 5) Eq a => [a] -> Bool
-- 6)
numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- d) binary machine language!
-- 7) Any type that implements `Ord` and `Num`!
-- 8) numbers :: (Ord a, Num a, Num b) => a -> b
-- Chapter Exercises
-- 1) A polymorphic funcion...
-- d) ...may resolve to values of different types, depending on inputs!
-- 2) f :: Char -> String . g :: String -> [String]?
-- b) g . f :: Char -> [String]
-- 3) f :: Ord a => a -> a -> a -> Bool
-- d) (Ord a, Num a) => a -> Bool
-- 4) (a -> b) -> c
-- b) is a higher order function!
-- 5) what is the type of f True?
ft :: a -> a
ft x = x

-- a) f True :: Bool
-- Let's write code
-- 1)
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = fst $ div x
    d = snd $ div xLast
    div = (flip divMod) 10

-- a) refactor using `divMod`!
-- b) obviously, it has the same type!
-- d) get the hundreds instead...
hunsD x = d2
  where
    d = fst $ x `divMod` 100
    d2 = snd $ d `divMod` 10

-- 2)
-- with case expressions
foldBool :: a -> a -> Bool -> a
foldBool x y z =
  case z of
    True  -> y
    False -> x

-- with guards!
foldBool' :: a -> a -> Bool -> a
foldBool' x y z
  | z == True = y
  | z == False = x

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
-- 4, 5, 6) see -> Arith4.hs!
