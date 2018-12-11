module Ackermann where

-- Ackermann function!
-- https://en.wikipedia.org/wiki/Ackermann_function

ackermann :: (Eq b, Num a, Num b, Ord a) => a -> b -> b
ackermann m n
  | m == 0          = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | otherwise       = ackermann (m - 1) (ackermann m (n - 1))
