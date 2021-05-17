{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables,
    FunctionalDependencies, FlexibleContexts #-}

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