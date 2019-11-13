{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

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

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (a, )
  (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State g =
    State $ \s ->
      let (fab, s') = f s
          (x, s'') = g s'
       in (fab x, s'')

instance Monad (State s) where
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= g =
    State $ \s ->
      let (a, s') = f s
          ms = runState (g a)
       in ms s'

-- chapter exercises
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put = State . const . ((), )

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ fmap f . ((), )

-- weirdest FizzBuzz possible as punishment
fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = exec (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to
  | from == to = fizzbuzzList [from]
  | from < to = fizzbuzzList [to,to - 1 .. from]
  | otherwise = fizzbuzzFromTo to from

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100
