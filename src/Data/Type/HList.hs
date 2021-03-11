{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables, FunctionalDependencies #-}

module Data.Type.HList (
    HList(..),
    hCombine,
    RearrangeList(..),
    RestructureList(..),
    TransformList(..),
    SubHList(..),
    FlattenToHList
    ) where

import Data.Type.Utils (Remove, Combine)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x :+: xs) = "(" ++ show x ++ ") : " ++ show xs

hCombine :: HList xs -> HList ys -> HList (Combine xs ys)
hCombine HNil ys = ys
hCombine (x :+: xs) ys = x :+: hCombine xs ys

-- GetHListElem, which finds an element of the type and returns the list
-- without that type.

class GetHListElem x inp out | x inp -> out where
    getHListElem :: HList inp -> (x, HList out)

instance {-# OVERLAPPING #-} GetHListElem x (x ': xs) xs where
    getHListElem (x :+: xs) = (x, xs)

instance (out ~ (o ': out'), GetHListElem x inp' out')
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
            where (elem, l') = getHListElem l :: (n, HList old')

-- SubHList, which gets the first n elements of a HList at type level.

type family TypeLen (list :: [*]) :: Nat where
    TypeLen '[] = 0
    TypeLen (HList x ': xs) = TypeLen x + TypeLen xs
    TypeLen (x ': xs) = 1 + TypeLen xs

type family FirstN (ty :: [*]) (n :: Nat) :: [*] where
    FirstN x 0 = '[]
    FirstN (x ': xs) n = x ': FirstN xs (n - 1)

type FirstNSucc x xs n = FirstN (x ': xs) n ~ (x ': FirstN xs (n-1))

type family AfterN (ty :: [*]) (n :: Nat) :: [*] where
    AfterN x 0 = x
    AfterN (x ': xs) n = AfterN xs (n-1)

type AfterNSucc x xs n = AfterN (x ': xs) n ~ AfterN xs (n-1)

class SubHList old (n :: Nat) where
    subHList :: HList old -> Proxy n -> (HList (FirstN old n), HList (AfterN old n))
    
instance {-# OVERLAPPING #-} SubHList old 0 where
    subHList xs _ = (HNil, xs)

-- FirstNSucc, AfterNSucc needed because we can guarantee that n > 0, but the
-- type checker cannot and thus can't perform the expansion ...NSucc does.
instance (old ~ (x ': xs), SubHList xs (n-1), FirstNSucc x xs n, AfterNSucc x xs n)
    => SubHList old n where
    subHList (o :+: os) _ = (o :+: before, after)
        where (before, after) = subHList os (Proxy :: Proxy (n-1))

-- RestructureList, which takes a flat HList and reshapes it.

class RestructureList old new where
    restructure :: HList old -> HList new

instance RestructureList '[] '[] where
    restructure _ = HNil

instance (len ~ TypeLen xs, RestructureList (FirstN old len) xs,
    RestructureList (AfterN old len) xss, SubHList old len)
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