module Chapter28 where

-- Basic libraries

import Criterion.Main (bench, defaultMain, nf, whnf)
import qualified Data.Map as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Vector as V

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

-- Set

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (0, 0)
    bumpIt (i, v) = (i + 1, v + 1)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where
    stream = iterate (+ 1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

-- Exercise: benchmark practice

insertionMap :: Int -> M.Map Int Int
insertionMap i = M.insert i i m

insertionSet :: Int -> S.Set Int
insertionSet i = S.insert i s

unionMap :: M.Map Int Int -> M.Map Int Int
unionMap = M.union m

unionSet :: S.Set Int -> S.Set Int
unionSet = S.union s

-- Sequence

lists :: [[Int]]
lists = replicate 10 myList

seqs :: [SQ.Seq Int]
seqs = replicate 10 (SQ.fromList myList)

seq' :: SQ.Seq Int
seq' = SQ.fromList myList

-- Vectors

slice :: Int -> Int -> [a] -> [a]
slice from len xs = take len (drop from xs)

v :: V.Vector Int
v = V.fromList myList

-- Exercise: vectors TODO:

-- Chapter exercises

-- DList!
newtype DList a = DL
  { unDL :: [a] -> [a]
  }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($ []) . unDL
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

-- Queue!
data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

newQ :: Queue a
newQ = Queue [] []

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue xs ys) = Queue (x : xs) ys

-- removes an item from the queue
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = pop (Queue [] (reverse xs))
pop (Queue xs (y : ys)) = Just (y, Queue xs ys)

main :: IO ()
main =
  defaultMain
    [ -- benchmark index access (unsafe/safe)
      bench "index list 9999" $ whnf (myList !!) 9998,
      bench "index list maybe index" $ whnf (myList !?) 9998,
      -- benchmark maps against lists
      bench "lookup one thing, list" $ whnf (lookup "notfound") pairList,
      bench "lookup one thing, map" $ whnf (M.lookup "notfound") testMap,
      -- benchmark maps against sets
      bench "member check map" $ whnf membersMap 9999,
      bench "member check set" $ whnf membersSet 9999,
      bench "insert check map" $ whnf insertionMap 9999,
      bench "insert check set" $ whnf insertionSet 9999,
      bench "union check map" $ whnf unionMap m,
      bench "union check set" $ whnf unionSet s,
      -- benchmark lists against sequences
      bench "concatenate lists" $ nf mconcat lists,
      bench "concatenate sequences" $ nf mconcat seqs,
      bench "indexing list" $ whnf (!! 9001) myList,
      bench "indexing sequence" $ whnf (`SQ.index` 9001) seq',
      -- benchmark lists against vectors!
      bench "slicing list" $ whnf (head . slice 100 900) myList,
      bench "slicing vector" $ whnf (V.head . V.slice 100 900) v,
      -- benchmark lists against dlists!
      bench "concat list" $ whnf schlemiel 123456,
      bench "concat dlist" $ whnf constructDlist 123456
    ]
