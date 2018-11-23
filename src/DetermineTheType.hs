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
