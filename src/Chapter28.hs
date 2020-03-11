module Chapter28 where

-- Basic libraries

import Criterion.Main
import qualified Data.Map as M

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

-- Maps

genList :: Int -> [(String, Int)]
genList n = go n []
  where
    go 0 xs = ("0", 0) : xs
    go n' xs =
      go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

main :: IO ()
main =
  defaultMain
    [ -- benchmark index access (unsafe/safe)
      bench "index list 9999" $ whnf (myList !!) 9998,
      bench "index list maybe index" $ whnf (myList !?) 9998,
      -- benchmark maps against lists
      bench "lookup one thing, list" $ whnf (lookup "notfound") pairList,
      bench "lookup one thing, map" $ whnf (M.lookup "notfound") testMap
    ]
