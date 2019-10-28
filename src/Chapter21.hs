module Chapter21 where

-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- Constant
newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = Yep <$> f x

-- List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

-- Pair
data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x $ f y

instance Foldable (Pair a) where
  foldMap f (Pair _ x) = f x

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

-- Big
data Big a b =
  Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ x y) = f x <> f y

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

-- Bigger
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  traverse f (Bigger w x y z) = Bigger w <$> f x <*> f y <*> f z

-- S
data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

-- Tree
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
