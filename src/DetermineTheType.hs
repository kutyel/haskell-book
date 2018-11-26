{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
-- example :: Num p => p
example = 1

-- a) Num a => a
a = (* 9) 6

-- b) Num a => (a, [Char])
b = head [(0, "doge"), (1, "kitteh")]

-- c) (Ingeger, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- d) Bool
d = if False then True else False

-- e) Int
e = length [1, 2, 3, 4, 5]

-- f) Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- Does it compile?

-- 1
bigNum = (^) 5
wahoo = bigNum $ 10

-- 2
x = print
y = print "woohoo!"
z = x "hello world"

-- 3
a1 = (+)
b1 = 5
c1 = a1 b1 10
d1 = a1 c1 200

-- 4
a2 b = 12 + b
b2 c = 10000 * c

-- Type variable or specific type constructor?

-- Legend:
-- FP -> Fully polymorphic
-- CP -> Constrained polymorphic
-- C -> Concrete

-- 1
-- f :: Num a => a -> b -> Int -> Int
--             [CP] [FP]   [C]   [C]

-- 2
-- f :: zed -> Zed -> Blah
--     [FP]    [C]    [C]

-- 3
-- f :: Enum b => a -> b -> Z
--              [FP] [CP]  [C]

-- 4
-- f :: f -> g -> C
--    [FP] [FP]  [C]

-- Write a type signature

-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y
