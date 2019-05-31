module Chapter14 where

half :: Fractional a => a -> a
half = (/ 2)

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, t)       = (Just y, x >= y)

associative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

commutative :: (Eq a) => (a -> a -> a) -> a -> a -> Bool
commutative f x y = x `f` y == y `f` x

quotAndRem :: Integral a => a -> a -> Bool
quotAndRem x y = (quot x y) * y + (rem x y) == x

divAndMod :: Integral a => a -> a -> Bool
divAndMod x y = (div x y) * y + (mod x y) == x
