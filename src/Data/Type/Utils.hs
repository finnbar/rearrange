{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Data.Type.Set (MemberP)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

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

type family HasDuplicates (xs :: [*]) :: Bool where
    HasDuplicates '[] = False
    HasDuplicates (x ': xs) = Or (Contains x xs) (HasDuplicates xs)

type family NoDuplicates (xs :: [*]) :: Constraint where
    NoDuplicates xs = NoDuplicatesHelper xs (HasDuplicates xs)

type family NoDuplicatesHelper (xs :: [*]) (c :: Bool) :: Constraint where
    NoDuplicatesHelper xs 'False = ()
    NoDuplicatesHelper xs 'True  =
        TypeError (Text "The list " :<>: ShowType xs :<>: Text " should not contain duplicates!")

data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b
type instance Eval (Foldl f acc '[]) = acc
type instance Eval (Foldl f acc (x ': xs)) =
    Eval (Foldl f (Eval (f acc x)) xs)