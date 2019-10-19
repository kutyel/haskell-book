{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lenses where

import           Data.Function                ((&))
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Generics.Product.Fields (field)
import           GHC.Generics

type Lens' s a
   = forall f. Functor f =>
                 (a -> f a) -> s -> f s

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
data Haskeller =
  Haskeller
    { hName       :: String
    , hExperience :: Int
    , hKnowledge  :: Knowledge
    }
  deriving (Generic, Show)

data Knowledge =
  Knowledge
    { kSyntax         :: Bool
    , kMonads         :: Bool
    , kLens           :: Bool
    , kTypeLevelMagic :: Bool
    , kNix            :: Bool
    }
  deriving (Generic, Show)

me :: Haskeller
me =
  Haskeller
    { hName = "Flavio"
    , hExperience = 1
    , hKnowledge =
        Knowledge
          { kSyntax = True
          , kMonads = True
          , kLens = False
          , kTypeLevelMagic = False
          , kNix = False
          }
    }

betterMe :: Haskeller
betterMe = me
  & field @"hName" .~ "Flavio, Haskeller"
  & field @"hKnowledge" . field @"kLens" .~ True
  & field @"hKnowledge" . field @"kTypeLevelMagic" .~ True
