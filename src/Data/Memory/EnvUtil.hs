{-# LANGUAGE UndecidableInstances, ExplicitForAll, FlexibleInstances,
    FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables #-}

module Data.Memory.EnvUtil (
    withEnv, withEnvM, growEnv
) where

import Data.Memory.Types (Cell(..), Memory, Set(..), Subset)
import MonadRW

import GHC.TypeLits
import Data.Type.Set (Union, Unionable, union, Proxy)
import MonadVar (MonadNew(..))
import Data.Default

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

class DefaultEnv env m where
    defaultEnv :: m (Set env)

instance Monad m => DefaultEnv '[] m where
    defaultEnv = return Empty

instance (DefaultEnv xs m, Monad m, MonadNew m v, Default t, MonadRW m v,
    Constr m v t) => DefaultEnv (Cell v s t ': xs) m where
        defaultEnv = do
            x <- new def
            Ext (Cell @s @t @m x) <$> defaultEnv

growEnv :: forall env env' xs m.
    (Union env xs ~ env', Unionable env xs, Monad m, DefaultEnv xs m) =>
    Set env -> m (Set env')
growEnv env = union env <$> defaultEnv @xs