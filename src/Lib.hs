module Lib
    ( sayHello
    ) where

-- Hello World!
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

-- First function
triple x = x * 3

-- Exercises: Comprehension Check

half x = x / 2

square x = x * x

-- Exercises: Heal the Sick

area x = pi * (x * x)

double x = x * 2

x = 7
y = 10
f = x + y

-- Exercises: A Head Code

mult1 = x * y
  where x = 5
        y = 6

-- 1. let x = 3; y = 1000 in x * 3 + y
mult2 = x * 3 + y
  where x = 3
        y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
mult3 = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
mult4 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- More fun with functions.
waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

waxOff x = triple x ^ 2 / 10
