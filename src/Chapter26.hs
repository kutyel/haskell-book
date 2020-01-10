module Chapter26 where

import Control.Arrow (first)

newtype MaybeT m a
  = MaybeT
      { runMaybeT :: m (Maybe a)
      }

instance (Functor f) => Functor (MaybeT f) where
  fmap f (MaybeT fma) = MaybeT $ (fmap . fmap) f fma

instance (Applicative f) => Applicative (MaybeT f) where

  pure = MaybeT . pure . pure

  MaybeT f <*> MaybeT a = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where

  return = pure

  MaybeT ma >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> pure Nothing
      Just x -> runMaybeT (f x)

-- exercises: EitherT
newtype EitherT e m a
  = EitherT
      { runEitherT :: m (Either e a)
      }

-- 1)
instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT fma) = EitherT $ (fmap . fmap) f fma

-- 2)
instance (Applicative m) => Applicative (EitherT e m) where

  pure = EitherT . pure . pure

  EitherT f <*> EitherT a = EitherT $ (<*>) <$> f <*> a

-- 3)
instance Monad m => Monad (EitherT e m) where

  return = pure

  EitherT me >>= f = EitherT $ do
    m <- me
    case m of
      Left e -> pure (Left e)
      Right a -> runEitherT (f a)

-- 4)
swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT x) = EitherT $ swapEither <$> x

-- 5)
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT me) = do
  m <- me
  case m of
    Left e -> f e
    Right a -> g a

-- ReaderT
newtype ReaderT r m a
  = ReaderT
      { runReaderT :: r -> m a
      }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where

  pure = ReaderT . pure . pure

  ReaderT fmab <*> ReaderT rma = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where

  return = pure

  ReaderT rma >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

-- exercises: StateT
newtype StateT s m a
  = StateT
      { runStateT :: s -> m (a, s)
      }

-- 1)
instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (fmap . fmap) (first f) sma

-- 2)
instance (Monad m) => Applicative (StateT s m) where

  pure a = StateT $ \s -> pure (a, s)

  StateT g <*> StateT h = StateT $ \s -> do
    (f, s') <- g s
    (a, s'') <- h s'
    pure (f a, s'')

-- 3)
instance (Monad m) => Monad (StateT s m) where

  return = pure

  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
