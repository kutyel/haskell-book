{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import           Control.Arrow (first)
import           System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

-- exercises: roll your own
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum count@(c, cs) gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (c + 1, intToDie die : cs) nextGen

-- write State yourself
newtype State s a =
  State
    { runState :: s -> (a, s)
    }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (first f . g)
