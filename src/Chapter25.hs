{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

{-# ANN module "HLint: ignore" #-}

-- twinplicative

newtype Compose f g a
  = Compose
      { getCompose :: f (g a)
      }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose $ ((<*>) <$> f) <*> a

-- compose instances

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- Bifunctors here
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1)
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

-- 2)
newtype Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const x) = Const $ f x

-- 3)
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

-- 4)
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

-- 5)
newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

-- 6)
data Quadriceps a b c d = Quadzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzz x y z z') = Quadzz x y (f z) (g z')

-- 7)
instance Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

-- my first Monad Transformer
newtype IdentityT f a
  = IdentityT
      { runIdentityT :: f a
      }
  deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ f <$> ma

instance Applicative m => Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure a = IdentityT $ pure a

  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  IdentityT mf <*> IdentityT ma = IdentityT $ mf <*> ma

instance Monad m => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  IdentityT ma >>= f = IdentityT $ ma >>= runIdentityT . f
