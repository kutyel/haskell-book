module Chapter18 where

import           Control.Monad (join)
import           Data.Bool     (bool)

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
