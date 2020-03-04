module Chapter27 where

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

-- 6) 'a' : 'b' : 'c' : 'd' : 'e' : [] -> 'a' `const` 'b' `const` 'c' `const` 'd' `const` 'e' `const` 'z' -> 'a'
ex6 :: Char
ex6 = foldr const 'z' ['a' .. 'e']

-- 7) 'a' : 'b' : 'c' : 'd' : 'e' : [] -> 'a' `fconst` 'b' `fconst` 'c' `fconst` 'd' `fconst` 'e' `fconst` 'z' -> 'z'
ex7 :: Char
ex7 = foldr (flip const) 'z' ['a' .. 'e']
-- Note on foldr: remember that `foldr` replaces `:` by `f`s and `[]` with `z`! (2nd argument)
