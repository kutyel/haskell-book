{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

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
  pure = pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose $ ((<*>) <$> f) <*> a

-- compose instances

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- TODO: Bifunctors here

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
