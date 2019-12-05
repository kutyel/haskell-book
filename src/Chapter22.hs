{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Monad (join)
import Data.Char
import Data.Maybe (fromMaybe)

newtype Reader r a
  = Reader
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
  Reader ra >>= f = join $ Reader (f . ra)

-- rewrite getDogRM with Reader!
newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person
  = Person
      { humanName :: HumanName,
        dogName :: DogName,
        address :: Address
      }

data Dog
  = Dog
      { dogsName :: DogName,
        dogsAddress :: Address
      }

getDogRM :: Reader Person Dog
getDogRM = Reader (Dog <$> dogName <*> address)

-- Chapter Exercises
x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7
  -- exercises
  print $ and $ sequA 6
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
