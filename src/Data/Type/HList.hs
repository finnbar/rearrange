{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables,
    FunctionalDependencies, FlexibleContexts #-}

module Data.Type.HList (
    HList(..),
    hCombine, hHead, hTail,
    RearrangeList(..),
    RestructureList(..),
    TransformList(..),
    SubHList(..),
    FlattenToHList,
    ) where

import Data.Type.Utils (Combine)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

-- Useful homogeneous list functions ported to HLists.

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x :+: xs) = "(" ++ show x ++ ") : " ++ show xs

hCombine :: HList xs -> HList ys -> HList (Combine xs ys)
hCombine HNil ys = ys
hCombine (x :+: xs) ys = x :+: hCombine xs ys

hHead :: HList (x ': xs) -> x
hHead (x :+: _) = x

hTail :: HList (x ': xs) -> HList xs
hTail (_ :+: xs) = xs

-- TODO: Neither of these work very well.

-- | ApplyHList takes a list of functions and applies e to each.
class ApplyHList fs e out | fs e -> out where
    hApply :: HList fs -> e -> HList out

instance ApplyHList '[] e '[] where
    hApply HNil _ = HNil

instance ApplyHList fs e outs =>
    ApplyHList ((e -> x) ': fs) e (x ': outs) where
        hApply (f :+: fs) e = f e :+: hApply fs e

-- | MapHList applies a function to a list of arguments.
-- TODO: this is currently extremely limited. Would be nice for it to use some
-- machinery to propagate constraints (e.g. for `hMap show`).
class MapHList xs f out | xs f -> out where
    hMap :: f -> HList xs -> HList out

instance MapHList '[] f '[] where
    hMap _ HNil = HNil

instance MapHList xs (a -> b) outs =>
    MapHList (a ': xs) (a -> b) (b ': outs) where
        hMap f (x :+: xs) = f x :+: hMap f xs

-- GetHListElem, which finds an element of the type and returns the list
-- without that type.

class GetHListElem x inp out | x inp -> out where
    getHListElem :: HList inp -> (x, HList out)

instance {-# OVERLAPPING #-} GetHListElem x (x ': xs) xs where
    getHListElem (x :+: xs) = (x, xs)

instance (GetHListElem x inp' out', out ~ (o ': out'))
    => GetHListElem x (o ': inp') out where
        getHListElem (y :+: xs) = (res, y :+: rest)
            where (res, rest) = getHListElem xs

-- RunComponents, which is a restricted version of map.

class RunComponents xs a where
    runComponents :: (a -> IO ()) -> HList xs -> IO ()

instance RunComponents '[] a where
    runComponents _ HNil = return ()

instance RunComponents xs a => RunComponents (a ': xs) a where
    runComponents f (x :+: xs) = do
        f x
        runComponents f xs

-- RearrangeList, which allows us to bring a type-level construct to values.

type family RearrangementError :: Constraint where
    RearrangementError =
        TypeError (Text "Could not rearrange between the two types provided." :$$:
            Text "Make sure that both types are rearrangements of one another!")

class RearrangeList old new where
    rearrange :: HList old -> HList new

instance RearrangeList '[] '[] where
    rearrange _ = HNil

instance RearrangementError => RearrangeList '[] (y ': ys) where
    rearrange _ = error "unreachable"

instance RearrangementError => RearrangeList (x ': xs) '[] where
    rearrange _ = error "unreachable"

instance (GetHListElem n old old', RearrangeList old' ns)
    => RearrangeList old (n ': ns) where
        rearrange l = elem :+: rearrange l'
            where (elem, l') = getHListElem l

-- SubHList, which gets the first n elements of a HList.

type family TypeLen (list :: [*]) :: Nat where
    TypeLen '[] = 0
    TypeLen (HList x ': xs) = TypeLen x + TypeLen xs
    TypeLen (x ': xs) = 1 + TypeLen xs

class SubHList old (n :: Nat) newl newr | old n -> newl newr where
    subHList :: HList old -> Proxy n -> (HList newl, HList newr)

instance {-# OVERLAPPING #-} SubHList xs 0 '[] xs where
    subHList list _ = (HNil, list)

instance (SubHList old' (n-1) newl' newr, newl ~ (x ': newl'), old ~ (x ': old'))
    => SubHList old n newl newr where
    subHList (o :+: os) _ = (o :+: left, right)
        where (left, right) = subHList os (Proxy :: Proxy (n-1))

-- RestructureList, which takes a flat HList and reshapes it.

class RestructureList old new where
    restructure :: HList old -> HList new

instance RestructureList '[] '[] where
    restructure _ = HNil

instance (len ~ TypeLen xs,
    RestructureList newl xs,
    RestructureList newr xss,
    SubHList old len newl newr)
    => RestructureList old (HList xs ': xss) where
    restructure list = restructure thisList :+: restructure rest
        where (thisList, rest) = subHList list (Proxy :: Proxy len)

instance RestructureList olds xss => RestructureList (x ': olds) (x ': xss) where
    restructure (l :+: ls) = l :+: restructure ls

-- TransformList, which applies both rearranging and reshaping.

class TransformList old new where
    transform :: HList old -> HList new

type family FLL (xs :: [*]) :: [*] where
    FLL '[] = '[]
    FLL (HList xs ': xss) = Combine (FLL xs) (FLL xss)
    FLL (x ': xs) = x ': FLL xs

instance (flat ~ FLL new, RearrangeList old flat, RestructureList flat new) =>
    TransformList old new where
    transform = restructure . (rearrange :: HList old -> HList flat)

-- FlattenToHList, which removes a layer of nesting by using HLists.

type family FlattenToHList (inp :: [[*]]) :: [*] where
    FlattenToHList '[] = '[]
    FlattenToHList (x ': xs) = HList x ': FlattenToHList xs