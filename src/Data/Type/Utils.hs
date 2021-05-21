-- A module containing type-level functions used in the rest of the code.
{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Fcf

data Elem :: k -> [k] -> Exp Bool
type instance Eval (Elem x '[]) = False
type instance Eval (Elem x (y ': ys)) =
  If (Eval (TyEq x y)) True (Eval (Elem x ys))

type NonEmptyInt xs ys = Eval (NonEmptyIntersect xs ys)
data NonEmptyIntersect :: [k] -> [k] -> Exp Bool
type instance Eval (NonEmptyIntersect xs ys) =
  Eval (Not =<< Null =<< Filter (Flip Elem xs) ys)

type Combine xs ys = Eval (xs ++ ys)

data CombinePair :: ([*], [*]) -> Exp [*]
type instance Eval (CombinePair '(xs, ys)) = Combine xs ys

type family Append (x :: k) (xs :: [k]) :: [k] where
    Append x '[] = '[x]
    Append x (y ': ys) = y ': Append x ys

data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b
type instance Eval (Foldl f acc '[]) = acc
type instance Eval (Foldl f acc (x ': xs)) =
    Eval (Foldl f (Eval (f acc x)) xs)

type family Without (xs :: [*]) (x :: *) :: [*] where
    Without '[] x = '[]
    Without (x ': xs) x = Without xs x
    Without (x ': xs) y = x ': Without xs y