{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lenses where

import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Identity
import Data.Generics.Product.Fields (field)
import GHC.Generics

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter f = fmap <$> setter <*> (f . getter)

-- view
view :: Lens' s a -> s -> a
view l = getConst . l Const

infixr 4 ^.

(^.) :: forall s a. s -> Lens' s a -> a
(^.) = flip @(Lens' s a) view

-- set
set :: Lens' s a -> a -> s -> s
set l a = runIdentity . l (const (Identity a))

infixr 4 .~

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

-- over
over :: Lens' s a -> (a -> a) -> s -> s
over l f = set l <$> f . view l <*> id

-- Usage
data Haskeller
  = Haskeller
      { name :: String,
        experience :: Int,
        knowledge :: Knowledge
      }
  deriving (Generic, Show)

data Knowledge
  = Knowledge
      { syntax :: Bool,
        monads :: Bool,
        lenses :: Bool,
        typeLevel :: Bool,
        nix :: Bool
      }
  deriving (Generic, Show)

me :: Haskeller
me =
  Haskeller
    { name = "Flavio",
      experience = 1,
      knowledge =
        Knowledge
          { syntax = True,
            monads = True,
            lenses = False,
            typeLevel = False,
            nix = False
          }
    }

betterMe :: Haskeller
betterMe =
  me
    & field @"experience" .~ 2
    & field @"name" .~ "Flavio, Haskeller"
    & field @"knowledge" . field @"lenses" .~ True
    & field @"knowledge" . field @"typeLevel" .~ True
