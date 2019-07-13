{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

import           GHC.Arr

-- 1) What is the kind of a? *
-- f :: a -> a
-- 2) What are the kinds of b and T? * -> *
-- f :: a -> b a -> T (b a)
-- 3) What is the kind of c? * -> * -> *
-- f :: c a b -> c b a
-- 1)
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2)
b = fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"])

-- 3)
c = (* 2) . (\x -> x - 2)

-- 4)
d = ((return '1' ++) . show) . (\x -> [x,1 .. 3])

-- 5)
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = (fmap read . fmap ("123" ++) . fmap show) ioi
   in fmap (* 3) changed

-- Trivial can't be made into a Functor because -> :k Trivial = * != * -> *
data Trivial =
  Trivial

-- Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Functor (Or a) where
  fmap _ (Fst x) = Fst x
  fmap f (Snd x) = Snd (f x)

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- Pair
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

-- Three'
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

-- Four
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z a) = Four x y z (f a)

-- Four'
data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z a) = Four' x y z (f a)

-- Possibly (Maybe)
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- Sum (Either)
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

-- Chapter exercises!
-- 1) 🚫 Cannot be a Functor, kind = *
-- 2) ✅ yes, kind = * -> *
data BoolAndSomethingElse a
  = False' a
  | True a

-- 3) ✅ yes, kind = * -> *
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- 4) ✅ yes, kind = (* -> *) -> *, but given * will be: * -> *
newtype Mu f =
  InF
    { outF :: f (Mu f)
    }

-- 5) 🚫 Cannot be a Functor, kind = *
data D =
  D (Array Word Word) Int Int

-- Rearrange Functors!
-- 1)
data Sum' b a
  = First' a
  | Second' b

instance Functor (Sum' e) where
  fmap f (First' a)  = First' (f a)
  fmap f (Second' b) = Second' b

-- 2)
data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3)
data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- write functor instances!
-- 1)
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant e) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- 2)
data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K e) where
  fmap _ (K x) = K x

-- 3)
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K (f x)

-- 4)
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst e) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5)
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6)
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa g x) = DaWrappa (fmap f g) (fmap f x)

-- 7)
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g e) where
  fmap f (IgnoringSomething g x) = IgnoringSomething g (fmap f x)

-- 8)
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap g (Notorious o a t) = Notorious o a (fmap g t)

-- 9)
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10)
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11)
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g)    = Read (f . g)
