-- A module containing type-level functions used in the rest of the code.
{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Data.Type.Set (MemberP)
import GHC.TypeLits
import Data.Kind

import Fcf (Exp, Eval)

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or x     y       = 'True

type family NonEmptyIntersect (xs :: [k]) (ys :: [k]) :: Bool where
    NonEmptyIntersect '[] ys       = 'False
    NonEmptyIntersect xs '[]       = 'False
    NonEmptyIntersect (x ': xs) ys = Or (MemberP x ys) (NonEmptyIntersect xs ys)

type family Contains (x :: k) (xs :: [k]) :: Bool where
    Contains x '[] = False
    Contains x (x ': xs) = True
    Contains x (y ': xs) = Contains x xs

type family Combine (xs :: [*]) (ys :: [*]) :: [*] where
    Combine '[] ys = ys
    Combine (x ': xs) ys = x ': Combine xs ys

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