module Chapter10 where

-- foldr f z [1, 2, 3]
-- 1 `f` (foldr f z [2, 3])
-- 1 `f` (2 `f` (foldr f z [3]))
-- 1 `f` (2 `f` (3 `f` (foldr f z [])))
-- 1 `f` (2 `f` (3 `f` z))

-- foldl (flip const) 0 [1..5]
-- f ~ (flip const); z ~ 0
-- (((((0 `f` 1) `f` 2) `f` 3) `f` 4) `f` 5)
-- ((((1 `f` 2) `f` 3) `f` 4) `f` 5)
-- (((2 `f` 3) `f` 4) `f` 5)
-- ((3 `f` 4) `f` 5)
-- (4 `f` 5)
-- 5

-- Understanding Folds

-- 1)
ex1 = foldr (*) 1 [1..5] == foldl (*) 1 [1..5] -- b) && c)

-- 2) foldl (flip (*)) 1 [1..3]
-- f ~ (flip (*)); z ~ 1
-- (((1 `f` 1) `f` 2) `f` 3)
-- (((1 * 1) * 2) * 3)
-- ((1 * 2) * 3)
-- (2 * 3)
-- 6

-- 3) one difference between `foldr` and `foldl` is:
-- c) `foldr`, but not `foldl`, associates to the right

-- 4) catamorphisms are usually used to...
-- a) ...reduce structure!

-- 5)
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) True [False, True] -- always returns `True`
e = foldl (flip ((++) . show)) "" [1..5]
f = foldr (flip const) 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]
