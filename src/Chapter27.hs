{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module Chapter27 where

import Debug.Trace

{-# ANN module "HLint: ignore" #-}

-- Laziness

-- Exercises: evaluate
-- 1) a -> b -> a > 1 -> b -> 1 > 1 -> undefined -> 1 > 1
ex1 :: Integer
ex1 = const 1 undefined

-- 2) a -> b -> a > undefined -> b -> undefined > undefined -> 1 -> undefined > *** Exception: Prelude.undefined
ex2 :: a
ex2 = const undefined 1

-- 3) a -> b -> b > undefined -> b -> b > undefined -> 1 -> 1 > 1
ex3 :: Integer
ex3 = flip const undefined 1

-- 4) a -> b -> b > 1 -> b -> b > 1 -> undefined -> undefined > *** Exception: Prelude.undefined
ex4 :: c
ex4 = flip const 1 undefined

-- 5) a -> b -> a > undefined -> b -> undefined > undefined -> undefined -> undefined > *** Exception: Prelude.undefined
ex5 :: a
ex5 = const undefined undefined

-- Note on foldr: remember that `foldr` replaces `:` by `f`s and `[]` with `z`! (2nd argument)
-- 6) 'a' : 'b' : 'c' : 'd' : 'e' : [] -> 'a' `const` 'b' `const` 'c' `const` 'd' `const` 'e' `const` 'z' -> 'a'
ex6 :: Char
ex6 = foldr const 'z' ['a' .. 'e']

-- 7) 'a' : 'b' : 'c' : 'd' : 'e' : [] -> 'a' `fconst` 'b' `fconst` 'c' `fconst` 'd' `fconst` 'e' `fconst` 'z' -> 'z'
ex7 :: Char
ex7 = foldr (flip const) 'z' ['a' .. 'e']

-- Implicit Parameters (language extension)

add :: (?x :: Int) => Int
add = trace "add" 1 + ?x

-- * Chapter27 Data.List Debug.Trace> let ?x = 2 in add
-- add
-- 3

-- Forcing sharing (Control.Monad.forever)
forever :: (Monad m) => m a -> m b
forever a = let a' = a >> a' in a'

-- Lazy patterns

strictPattern :: (a, b) -> String
strictPattern (a, b) = const "Cousing It" a

-- * Chapter27 Data.List Debug.Trace> strictPattern undefined
-- "*** Exception: Prelude.undefined

lazyPattern :: (a, b) -> String
lazyPattern ~(a, b) = const "Cousing It" a

-- * Chapter27 Data.List Debug.Trace> lazyPattern undefined
-- "Cousing It"

-- Bang patterns

banging :: Bool -> Int
banging !b = 1 -- `b` always gets evaluated! (like with `seq`)

-- Chapter exercises

-- Strict list: LANGUAGE Strict == Cons !a !(List a)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

take' :: (Ord n, Num n) => n -> List a -> List a
take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n -1) xs)

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' :: a -> List a
repeat' x = xs where xs = Cons x xs

main :: IO ()
main = print $ take' 10 $ map' (+ 1) (repeat' 1)
