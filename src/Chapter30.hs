{-# LANGUAGE GADTs #-}

module Chapter30 where

import Control.Exception (ArithException (..), AsyncException (..))
import Data.Typeable

data MyException
  = forall e. (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

data SomeError
  = Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  maybe (maybe SomethingElse Async (cast e)) Arith (cast e)

runDisc :: Int -> SomeError
runDisc n =
  either discriminateError (const SomethingElse) (multiError n)
