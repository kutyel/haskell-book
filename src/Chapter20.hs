module Chapter20 where

import           Control.Conditional (select)
import           Data.Monoid

-- :t foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- :t foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- library functions
-- 1)
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- 2)
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3)
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (== x))

-- 4)
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f x Nothing  = Just x
    f x (Just y) = Just (min x y)

-- 5)
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f x Nothing  = Just x
    f x (Just y) = Just (max x y)

-- 6)
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7)
length :: (Foldable t) => t a -> Int
length = foldr (const (+ 1)) 0

-- 8)
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9)
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10)
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Chapter Exercises
-- 1)
newtype Constant a b =
  Constant b

-- 2)
data Two a b =
  Two a b

-- 3)
data Three a b c =
  Three a b c

-- 4)
data Three' a b =
  Three' a b b

-- 5)
data Four' a b =
  Four' a b b b

-- extra (tip: select = functional if-then-else)
filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (select f pure mempty)
