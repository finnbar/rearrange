{-# LANGUAGE UndecidableInstances, ExplicitForAll #-}

module Data.Memory.WithEnv (
    withEnv, withEnvM
) where

import Data.Memory.Types
import Data.Type.HList

import GHC.TypeLits

type family LookupOne (env :: [*]) (x :: *) :: * where
    LookupOne '[] x = TypeError (Text "Unable to find " :<>: ShowType x :<>:
        Text " in the provided environment." :$$:
        Text "Check whether it is present!")
    LookupOne (Cell v s t ': _) (Cell _ s _) = Cell v s t
    LookupOne (Cell _ _ _ ': es) x = LookupOne es x

type family LookupSpecific (env :: [*]) (xs :: [*]) :: [*] where
    LookupSpecific env '[] = '[]
    LookupSpecific env (x ': xs) = LookupOne env x ': LookupSpecific env xs

withEnv :: forall l m env rs ws a.
    (rs ~ LookupSpecific env rs, ws ~ LookupSpecific env ws) =>
    Set env -> Memory m l '(rs, ws) a -> Memory m l '(rs, ws) a
withEnv _ = id
{-# INLINE withEnv #-}

withEnvM :: forall l m env rs ws a.
    (rs ~ LookupSpecific env rs, ws ~ LookupSpecific env ws) =>
    m (Set env) -> Memory m l '(rs, ws) a -> Memory m l '(rs, ws) a
withEnvM _ = id
{-# INLINE withEnvM #-}