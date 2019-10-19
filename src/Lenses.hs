{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Lenses where

import           Control.Lens.TH       (makeLenses)
import           Data.Function         ((&))
import           Data.Functor.Const
import           Data.Functor.Identity

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
    { _hName       :: String
    , _hExperience :: Int
    , _hKnowledge  :: Knowledge
    }
  deriving (Show)

data Knowledge =
  Knowledge
    { _kSyntax         :: Bool
    , _kMonads         :: Bool
    , _kLens           :: Bool
    , _kTypeLevelMagic :: Bool
    , _kNix            :: Bool
    }
  deriving (Show)

$(makeLenses ''Haskeller)
$(makeLenses ''Knowledge)

me :: Haskeller
me =
  Haskeller
    { _hName = "Flavio"
    , _hExperience = 1
    , _hKnowledge =
        Knowledge
          { _kSyntax = True
          , _kMonads = True
          , _kLens = False
          , _kTypeLevelMagic = False
          , _kNix = False
          }
    }

betterMe :: Haskeller
betterMe = me
  & hName .~ "Flavio, Haskeller"
  & hKnowledge . kLens .~ True
  & hKnowledge . kTypeLevelMagic .~ True
