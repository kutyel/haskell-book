module Lib
  ( sayHello
  ) where

-- Hello World!
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

-- First function
triple :: Num a => a -> a
triple x = x * 3

-- Exercises: Comprehension Check
half :: Fractional a => a -> a
half x = x / 2

square :: Int -> Int
square x = x * x

-- Exercises: Heal the Sick
area :: Floating a => a -> a
area x = pi * (x * x)

double :: Int -> Int
double x = x * 2

x = 7

y = 10

f = x + y

-- Exercises: A Head Code
mult1 :: Int
mult1 = x * y
  where
    x = 5
    y = 6

-- 1. let x = 3; y = 1000 in x * 3 + y
mult2 :: Int
mult2 = x * 3 + y
  where
    x = 3
    y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
mult3 :: Int
mult3 = x * 5
  where
    y = 10
    x = 10 * 5 + y

-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
mult4 :: Double
mult4 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

-- More fun with functions.
waxOn :: Integer
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

waxOff :: Fractional a => a -> a
waxOff x = triple x ^ 2 / 10
