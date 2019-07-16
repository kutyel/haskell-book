module Chapter17 where

import           Control.Applicative
import           Data.List           (elemIndex)

f :: (Eq a, Num a) => a -> Maybe String
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g :: (Eq a, Num a) => a -> Maybe String
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h :: (Eq a, Num a) => a -> Maybe a
h z = lookup z [(2, 3), (5, 6), (7, 8)]

m :: (Eq a, Num a) => a -> Maybe a
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

foo :: Maybe String
foo = (++) <$> f 3 <*> g 7

l :: Maybe Int
l = fmap length foo

-- lookups
-- 1)
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2)
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3)
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4)
xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''

-- Identity Instance
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)
