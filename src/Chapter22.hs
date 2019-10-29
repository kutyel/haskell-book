{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import           Control.Monad (join)
import           Data.Char

newtype Reader r a =
  Reader
    { runReader :: r -> a
    }

-- warming up!
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

-- monads, monads everywhere!
tupled' :: String -> (String, String)
tupled' = do
  reversed <- rev
  capitalised <- cap
  return (reversed, capitalised)

tupled'' :: String -> (String, String)
tupled'' = rev >>= \r -> cap >>= \c -> pure (r, c)

-- Ask
ask :: Reader a a
ask = Reader id

-- Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ pure a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

-- Reader Monad
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = join $ Reader $ \r -> aRb (ra r)

-- rewrite getDogRM with Reader!
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    }

data Dog =
  Dog
    { dogsName    :: DogName
    , dogsAddress :: Address
    }

getDogRM :: Reader Person Dog
getDogRM = Reader (Dog <$> dogName <*> address)
