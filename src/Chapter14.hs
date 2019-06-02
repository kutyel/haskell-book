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

prop_half :: (Eq a, Fractional a) => a -> Bool
prop_half x = half x * 2 == x

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

prop_reverseTwice :: Eq a => [a] -> Bool
prop_reverseTwice x = (reverse . reverse) x == id x

prop_applyOperator :: Eq a => a -> Bool
prop_applyOperator x = id $ x == id x

prop_composition :: Eq a => a -> Bool
prop_composition x = (id . id) x == id (id x)

prop_roundTrip :: (Eq a, Read a, Show a) => a -> Bool
prop_roundTrip x = (read . show) x == x

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
