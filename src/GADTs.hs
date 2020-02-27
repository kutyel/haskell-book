{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GADTs where

data Nat = Z | S Nat

one :: Nat
one = S Z

three :: Nat
three = S (S (S Z))

data Vec (n :: Nat) a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data Tree (n :: Nat) a where
  Leaf :: a -> Tree (S Z) a
  Node :: SNat n -> Tree n a -> Tree m a -> Tree (n :+: m) a

type family (n :: Nat) :+: (m :: Nat) where
  Z :+: m = m
  S n :+: m = S (n :+: m)

leaves :: Tree n a -> Vec n a
leaves (Leaf x) = VCons x VNil
leaves (Node _ l r) = leaves l `append` leaves r

append :: Vec n a -> Vec m a -> Vec (n :+: m) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (xs `append` ys)

update :: Tree n a -> Vec n b -> Tree n b
update (Leaf x) (VCons y VNil) = Leaf y
update (Node i l r) leaves =
  let (leftl, rightl) = split i leaves
   in Node i (update l leftl) (update r rightl)

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

split :: (t ~ (n :+: m)) => SNat n -> Vec t a -> (Vec n a, Vec m a)
split SZ xs = (VNil, xs)
split (SS n) (VCons x xs) =
  let (xs1, xs2) = split n xs
   in (VCons x xs1, xs2)
