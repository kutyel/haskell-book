module Chapter12 where

import Data.Char (toLower)
import Data.List (partition)

-- Determine the kinds
-- 1) id :: a -> a ? * -> *
-- 2) r :: a -> f a ? a = *, f = * -> *
-- String processing
-- 1
notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe = unwords . map replace . words
  where
    replace x = fromMaybe "a" (notThe x)

-- 2
isVowel :: Char -> Bool
isVowel = flip elem "aeiou" . toLower

startsWithVowel :: String -> Bool
startsWithVowel = isVowel . head

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0
  where
    go [_] count = count
    go (x : y : xs) count =
      go
        (y : xs)
        (if x == "the" && startsWithVowel y then count + 1 else count)

-- 3
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

-- validate the word
newtype Word'
  = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | length vs > length cs = Nothing
  | otherwise = Just (Word' s)
  where
    (vs, cs) = partition isVowel s

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (toNat x)
  where
    toNat 0 = Zero
    toNat n = Succ (toNat (n - 1))

-- Small library for Maybe
-- 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise = Just (catMaybes xs)

-- Small library for Either
-- 1
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) xs = x : xs
    f _ xs = xs

-- 2
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) xs = x : xs
    f _ xs = xs

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

-- 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Write your own iterate and unfoldr
-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b

-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Finally something other than a list!
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

-- 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    ( \x ->
        if x == n
          then Nothing
          else Just (x + 1, x, x + 1)
    )
    0
