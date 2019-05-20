module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 x) ++ (take 4 (drop 5 x)) ++ (take 5 x)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if x > 0
    then x
    else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x :: Num a => a -> a -> a
x = (+)

g :: Foldable t => t a -> Int
g xs = w `x` 1
  where
    w = length xs

main :: IO ()
main = print $ rvrs "Curry is awesome"
