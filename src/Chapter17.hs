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
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

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

-- Constant Instance
newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure = Constant . mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)

-- Fixer Upper
-- 1)
ex1 = const <$> Just "Hello" <*> pure "World"

-- 2)
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Reason tribute
data Option a
  = None
  | Some a
  deriving (Eq, Show)

instance Functor Option where
  fmap _ None     = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  pure = Some
  None <*> _ = None
  _ <*> None = None
  (Some f) <*> (Some x) = Some (f x)

-- List applicative
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) `append` (fs <*> xs)

-- hints
append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

toMyList :: Foldable t => t a -> List a
toMyList = foldr Cons Nil

-- ZipList applicative
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . flip Cons Nil
  x <*> y =
    case (x, y) of
      (_, ZipList' Nil) -> ZipList' Nil
      (ZipList' Nil, _) -> ZipList' Nil
      (ZipList' (Cons f Nil), ZipList' (Cons x xs)) ->
        ZipList' $ Cons (f x) (pure f <*> xs)
      (ZipList' (Cons f fs), ZipList' (Cons x Nil)) ->
        ZipList' $ Cons (f x) (fs <*> pure x)
      (ZipList' (Cons f fs), ZipList' (Cons x xs)) ->
        ZipList' $ Cons (f x) (fs <*> xs)
