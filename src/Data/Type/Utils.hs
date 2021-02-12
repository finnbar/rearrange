{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Data.Type.Set (MemberP)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

import Fcf (Exp, Eval)

type family Remove (x :: k) (xs :: [k]) :: [k] where
    Remove x '[] = '[]
    Remove x (x ': xs) = xs
    Remove x (y ': xs) = y : Remove x xs

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or x     y       = 'True

type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'True 'True = 'True
    And x     y     = 'False

type family NonEmptyIntersect (xs :: [k]) (ys :: [k]) :: Bool where
    NonEmptyIntersect '[] ys       = 'False
    NonEmptyIntersect xs '[]       = 'False
    NonEmptyIntersect (x ': xs) ys = Or (MemberP x ys) (NonEmptyIntersect xs ys)

type family If (b :: Bool) (x :: k) (y :: k) :: k where
    If 'True x y  = x
    If 'False x y = y

type family Fst (x :: (k, l)) :: k where
    Fst '(x, y) = x
type family Snd (x :: (k, l)) :: l where
    Snd '(x, y) = y

type family Contains (x :: k) (xs :: [k]) :: Bool where
    Contains x '[] = False
    Contains x (x ': xs) = True
    Contains x (y ': xs) = Contains x xs

type family Contains2D (x :: k) (xs :: [[k]]) :: Bool where
    Contains2D x '[] = False
    Contains2D x (y ': ys) = Or (Contains x y) (Contains2D x ys)

type family Combine (xs :: [*]) (ys :: [*]) :: [*] where
    Combine '[] ys = ys
    Combine (x ': xs) ys = x ': Combine xs ys

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

class AsBool (b :: Bool) where
    asBool :: Proxy b -> Bool

instance AsBool True where
    asBool _ = True

instance AsBool False where
    asBool _ = False