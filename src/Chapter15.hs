module Chapter15 where

import           Data.Monoid

-- Optional Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend a b =
    case (a, b) of
      (Nada, Nada)     -> Nada
      (Only x, Nada)   -> Only x
      (Nada, Only x)   -> Only x
      (Only x, Only y) -> Only (x <> y)
