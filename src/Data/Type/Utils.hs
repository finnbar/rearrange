{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Data.Type.Set (MemberP)
import GHC.TypeLits
import Data.Kind

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

type family NonEmptyIntersect (xs :: [*]) (ys :: [*]) :: Bool where
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

type family HasDuplicates (xs :: [*]) :: Bool where
    HasDuplicates '[] = False
    HasDuplicates (x ': xs) = Or (Contains x xs) (HasDuplicates xs)

type family NoDuplicates (xs :: [*]) :: Constraint where
    NoDuplicates xs = NoDuplicatesHelper xs (HasDuplicates xs)

type family NoDuplicatesHelper (xs :: [*]) (c :: Bool) :: Constraint where
    NoDuplicatesHelper xs 'False = ()
    NoDuplicatesHelper xs 'True  =
        TypeError (Text "The list " :<>: ShowType xs :<>: Text " should not contain duplicates!")