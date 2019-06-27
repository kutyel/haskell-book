module Chapter15 where

import           Data.Monoid

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

-- Combine
newtype Combine a b =
  Combine
    { unCombine :: (a -> b)
    }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

-- Comp
newtype Comp a =
  Comp
    { unComp :: (a -> a)
    }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

-- Look familiar? -> Either!
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) a b =
    case (a, b) of
      (Success x, Failure _) -> Success x
      (Failure _, Success x) -> Success x
      (Success x, Success _) -> Success x
      (Failure x, Failure y) -> Failure (x <> y)

main = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
