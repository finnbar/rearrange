{-# LANGUAGE UndecidableInstances, PolyKinds #-}

module Data.Type.Utils where

import Data.Type.Set (MemberP)

type family Not (x :: Bool) :: Bool where
    Not True = False
    Not False = True

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