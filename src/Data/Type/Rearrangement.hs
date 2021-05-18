{-# LANGUAGE UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes, ExplicitForAll,
    FunctionalDependencies, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}

module Data.Type.Rearrangement (
    rearrangeDel, RearrangeDel, permute, Permute, Rearrange(..)
) where

import Data.Type.HList

-- First pass - specialised to HLists.

rearrangeDel :: RearrangeDel env target env' => HList env -> HList target
rearrangeDel = fst . rDel

type Permute env target = RearrangeDel env target '[]
permute :: Permute env target => HList env -> HList target
permute = rearrangeDel

class RearrangeDel env target env' | env target -> env' where
    rDel :: HList env -> (HList target, HList env')

instance RearrangeDel env '[] env where
    rDel l = (HNil, l)

instance {-# OVERLAPPABLE #-} (RearrangeDel env' target' env'', GetHListElem x env env') =>
    RearrangeDel env (x ': target') env'' where
        rDel l = (x :+: xs, l'')
            where (x, l') = getHListElem l
                  (xs, l'') = rDel l'

instance {-# OVERLAPPING #-} (RearrangeDel env head env', RearrangeDel env' target' env'') =>
    RearrangeDel env (HList head ': target') env'' where
        rDel l = (head' :+: tail', l'')
            where (head', l') = rDel l
                  (tail', l'') = rDel l'

class Rearrange env target where
    rearrange :: HList env -> HList target

instance Rearrange env '[] where
    rearrange _ = HNil

instance {-# OVERLAPPABLE #-} (Rearrange env target, GetHListElem x env env') =>
    Rearrange env (x ': target) where
        rearrange l = x :+: rearrange l
            where (x, _) = getHListElem l

instance {-# OVERLAPPING #-} (Rearrange env head, Rearrange env tail) =>
    Rearrange env (HList head ': tail) where
        rearrange l = rearrange l :+: rearrange l

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

-- HTraversable, for generalising heterogeneous traversals.

class HUpdate c t env t' env' | c env t t' -> env' where
    hUpdate :: (c t env t' env') => t -> env -> (t', env')

type HUpd c env t t' env' = (HUpdate c env t t' env', c env t t' env')

class HTraversable upd st env st' env' where
    -- Takes in some structure st
    -- and an env
    -- to the updated structure and updated env
    -- (note that HUpdate is used to generate st', env')
    hTraverse :: st -> env -> (st', env')

-- For a non-container type, and a function that works on it.
instance {-# OVERLAPPABLE #-} (HUpd upd env t t' env') =>
    HTraversable upd t env t' env' where
    -- t -> (env -> t -> (t', env')) -> env -> (t', env')
    hTraverse inp a = hUpdate @upd a inp

-- For the empty list.
instance HTraversable upd (HList '[]) a (HList '[]) a where
    hTraverse _ a = (HNil, a)

-- For non-empty lists.
instance {-# OVERLAPPING #-}
    (HTraversable upd x env y env',
    HTraversable upd (HList xs) env' (HList ys) env'') =>
    HTraversable upd (HList (x ': xs)) env (HList (y ': ys)) env'' where
        hTraverse (x :+: xs) a = (y :+: ys, a'')
            where (y, a') = hTraverse @upd @_ @env @_ @env' x a
                  (ys, a'') = hTraverse @upd xs a'

-- toHList, which converts any heterogeneous structure to a HList

instance (ConcatToHList env t t' env') => HUpdate ConcatToHList env t t' env' where
    hUpdate = concatToHList

class ConcatToHList t xs t' xs' | xs t t' -> xs' where
    concatToHList :: t -> xs -> (t', xs')

instance ConcatToHList t (HList xs) t (HList (t ': xs)) where
    concatToHList x xs = (x, x :+: xs)

toHList :: forall st st' xs. HTraversable ConcatToHList st (HList '[]) st' (HList xs) => st -> HList xs
toHList st = snd $ hTraverse @ConcatToHList @st @_ @st' st HNil