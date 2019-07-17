module Chapter15 where

import           Data.Monoid

-- Trivial
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two a b) = Two (x <> a) (y <> b)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three a b c) = Three (x <> a) (y <> b) (z <> c)

-- Four
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four w x y z) <> (Four a b c d) = Four (w <> a) (x <> b) (y <> c) (z <> d)

-- Bull
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  x <> y =
    case (x, y) of
      (Fst _, Snd x) -> Snd x
      (Fst _, Fst x) -> Fst x
      (Snd x, Fst _) -> Snd x
      (Snd x, Snd _) -> Snd x

-- First'
newtype First' a =
  First'
    { getFirst' :: Optional a
    }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  a <> b =
    case (getFirst' a, getFirst' b) of
      (Nada, Nada) -> First' Nada
      (Only x, _)  -> First' (Only x)
      (_, Only x)  -> First' (Only x)

instance Monoid (First' a) where
  mempty = First' Nada

-- Optional Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) a b =
    case (a, b) of
      (Nada, Nada)     -> Nada
      (Only x, Nada)   -> Only x
      (Nada, Only x)   -> Only x
      (Only x, Only y) -> Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- Madness
type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

-- BoolConj (and)
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y =
    case (x, y) of
      (True, True) -> BoolConj True
      (_, _)       -> BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

-- BoolDisj (or)
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) =
    case (x, y) of
      (_, True) -> BoolDisj True
      (True, _) -> BoolDisj True
      (_, _)    -> BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False

-- Combine
newtype Combine a b =
  Combine
    { unCombine :: (a -> b)
    }

instance Show (Combine a b) where
  show _ = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

-- Comp
newtype Comp a =
  Comp
    { unComp :: (a -> a)
    }

instance Show (Comp a) where
  show _ = "Comp"

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty

-- Look familiar? -> Either!
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  a <> b =
    case (a, b) of
      (Success x, Failure _) -> Success x
      (Failure _, Success x) -> Success x
      (Success x, Success _) -> Success x
      (Failure x, Failure y) -> Failure (x <> y)

-- Mem
newtype Mem s a =
  Mem
    { runMem :: s -> (a, s)
    }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g =
    Mem $ \x ->
      let (a, b) = g x
          (c, d) = f b
       in (a <> c, d)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

main = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
      f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ success 1 <> failure "blah" -- Success 1
  print $ failure "woot" <> failure "blah" -- Failure "wootblah"
  print $ success 1 <> success 2 -- Success 1
  print $ failure "woot" <> success 2 -- Success 2
  print $ "Mem ----------------------"
  print $ rmleft -- ("hi", 1)
  print $ rmright -- ("hi", 1)
  print $ (rmzero :: (String, Int)) -- ("", 0)
  print $ rmleft == runMem f' 0 -- True
  print $ rmright == runMem f' 0 -- True
