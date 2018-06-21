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
