module Chapter18 where

import           Control.Applicative (liftA2)
import           Control.Monad       (join)
import           Data.Bool           (bool)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- List Monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs =
  xs >>= \x ->
    let y = x * x
     in bool [y] [y, y] $ even x

-- Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second x = Second $ f x

instance Monad (Sum a) where
  return = pure
  First x >>= _ = First x
  Second x >>= f = f x

-- Chapter exercises
-- 1)
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

-- 2)
data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight x) = PRight x
  fmap f (PLeft x)  = PLeft $ f x

instance Applicative (BahEither b) where
  pure = PLeft
  PRight x <*> _ = PRight x
  _ <*> PRight x = PRight x
  PLeft f <*> PLeft x = PLeft $ f x

instance Monad (BahEither b) where
  return = pure
  PRight x >>= _ = PRight x
  PLeft x >>= f = f x

-- 3)
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

-- 4)
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil xs         = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (f <$> xs) `append` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x `append` (xs >>= f)

-- Implement the following methods
-- 1) j == join
j :: Monad m => m (m a) -> m a
j = (=<<) id

-- 2) l2 == fmap (f <$> x)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3) l2 == ap (f <$> x <*> y)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4) a == <*> (fs <*> xs)
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5) meh == for == flip traverse == (sequence . fmap f)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = f x >>= \x' -> (x' :) <$> meh xs f

-- 6) flipType == traverse
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
