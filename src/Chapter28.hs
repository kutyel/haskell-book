module Chapter28 where

-- Basic libraries

import Criterion.Main

-- safe access operator
infixl 0 !?

{-# INLINEABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      ( \x r k ->
          case k of
            0 -> Just x
            _ -> r (k - 1)
      )
      (const Nothing)
      xs
      n

myList :: [Int]
myList = [1 .. 9999]

main :: IO ()
main =
  defaultMain
    [ -- benchmark index access (unsafe/safe)
      bench "index list 9999" $ whnf (myList !!) 9998,
      bench "index list maybe index" $ whnf (myList !?) 998
    ]
