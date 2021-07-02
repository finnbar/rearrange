-- This module implements withEnvM (unifying the types in the environment with
-- the types in the computation to reduce the need for type annotations) and
-- the automatic allocation of AutoCells (to avoid having to state them in the
-- environment).

{-# LANGUAGE UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes,
    ScopedTypeVariables, FunctionalDependencies, FlexibleContexts #-}

module Data.Memory.EnvUtil (
    withEnv, withEnvM, AddAutoCells(..), AAC, WithoutInters, GetEnvFromMems, retrieve
) where

import Data.Memory.Types (Cell(..), Memory, Set(..), Subset, AutoCell(..))

import GHC.TypeLits
import Data.Type.Set (Union, Unionable, union, subset)
import MonadVar (MonadNew(..))
import Data.Default (Default(..))
import Data.IORef (newIORef)

type family LookupOne (env :: [*]) (x :: *) :: * where
    LookupOne _ (AutoCell s t) = AutoCell s t
    LookupOne '[] x = TypeError (Text "Unable to find " :<>: ShowType x :<>:
        Text " in the provided environment." :$$:
        Text "Check whether it is present!")
    LookupOne (Cell v s t ': _) (Cell _ s _) = Cell v s t
    LookupOne (Cell _ _ _ ': es) x = LookupOne es x

type family LookupSpecific (env :: [*]) (xs :: [*]) :: [*] where
    LookupSpecific env '[] = '[]
    LookupSpecific env (x ': xs) = LookupOne env x ': LookupSpecific env xs

withEnv :: (rs ~ LookupSpecific env rs, ws ~ LookupSpecific env ws) =>
    Set env -> Memory m '(rs, ws) a -> Memory m '(rs, ws) a
withEnv _ = id
{-# INLINE withEnv #-}

withEnvM :: (rs ~ LookupSpecific env rs, ws ~ LookupSpecific env ws) =>
    m (Set env) -> Memory m '(rs, ws) a -> Memory m '(rs, ws) a
withEnvM _ = id
{-# INLINE withEnvM #-}

type AAC env env' mems = (AddAutoCells env env', env ~ WithoutInters env',
    env' ~ GetEnvFromMems mems)

class AddAutoCells (env :: [k]) (env' :: [k]) | env' -> env where
    addAutoCells :: Set env -> IO (Set env')

instance AddAutoCells '[] '[] where
    addAutoCells Empty = return Empty

instance {-# OVERLAPPING #-} AddAutoCells xs xs' =>
    AddAutoCells (x ': xs) (x ': xs') where
        addAutoCells (Ext x xs) = Ext x <$> addAutoCells xs

instance {-# OVERLAPPABLE #-} (AddAutoCells xs xs', Default t) =>
    AddAutoCells xs (AutoCell s t ': xs') where
        addAutoCells xs = do
            cell <- AutoCell @s @t <$> newIORef def
            cells <- addAutoCells xs
            return $ Ext cell cells

type family WithoutInters (xs :: [*]) :: [*] where
    WithoutInters '[] = '[]
    WithoutInters (AutoCell _ _ ': xs) = WithoutInters xs
    WithoutInters (x ': xs) = x ': WithoutInters xs

type family GetEnvFromMems (xs :: [*]) :: [*] where
    GetEnvFromMems '[] = '[]
    GetEnvFromMems (Memory _ '(rs, ws) _ ': xs) =
        Union rs (Union ws (GetEnvFromMems xs))

type family LookupVT (xs :: [*]) (s :: Symbol) :: (* -> *, *) where
    LookupVT (Cell v s t ': xs) s = '(v, t)
    LookupVT (x ': xs) s = LookupVT xs s

retrieve :: forall s v t xs xs'. (Subset xs' xs, xs' ~ '[Cell v s t], '(v, t) ~ LookupVT xs s) =>
    Set xs -> Cell v s t
retrieve set = case subset @xs' @xs set of
    Ext x _ -> x