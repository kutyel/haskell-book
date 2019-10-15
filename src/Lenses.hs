{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}

module Lenses where

import           Control.Applicative   (liftA2)
import           Data.Functor.Const
import           Data.Functor.Identity

type Lens' s a
   = forall f. Functor f =>
                 (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter f = liftA2 fmap setter (f . getter)

-- view
view :: Lens' s a -> s -> a
view l = getConst . l Const

-- set
set :: Lens' s a -> a -> s -> s
set l a = runIdentity . l (const (Identity a))

over :: Lens' s a -> (a -> a) -> s -> s
over l f = liftA2 (set l) (f . view l) id

-- Usage
data Haskeller =
  Haskeller
    { haskellerName       :: String
    , haskellerExperience :: Int
    , haskellerKnowledge  :: Knowledge
    }
  deriving (Show)

data Knowledge =
  Knowledge
    { kSyntax         :: Bool
    , kMonads         :: Bool
    , kLens           :: Bool
    , kTypeLevelMagic :: Bool
    , kNix            :: Bool
    }
  deriving (Show)

me :: Haskeller
me =
  Haskeller
    { haskellerName = "Flavio"
    , haskellerExperience = 1
    , haskellerKnowledge =
        Knowledge
          { kSyntax = True
          , kMonads = True
          , kLens = False
          , kTypeLevelMagic = False
          , kNix = False
          }
    }

nameL :: Lens' Haskeller String
nameL = lens getter setter
  where
    getter :: Haskeller -> String
    getter = haskellerName
    setter :: Haskeller -> String -> Haskeller
    setter h newName = h {haskellerName = newName}
