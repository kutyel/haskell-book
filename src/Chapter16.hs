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
