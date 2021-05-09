{-# LANGUAGE UndecidableInstances, ExplicitForAll, FlexibleInstances,
    FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies #-}

module Data.Memory.EnvUtil (
    withEnv, withEnvM, AddInterCells(..), WithoutInters, GetEnvFromMems
) where

import Data.Memory.Types (Cell(..), Memory, Set(..), Subset, InterCell(..))
import MonadRW

import GHC.TypeLits
import Data.Type.Set (Union, Unionable, union)
import MonadVar (MonadNew(..))
import Data.Default
import Data.IORef

type family LookupOne (env :: [*]) (x :: *) :: * where
    LookupOne _ (InterCell s t) = InterCell s t
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

class AddInterCells (env :: [k]) (env' :: [k]) | env' -> env where
    addInterCells :: Set env -> IO (Set env')

instance AddInterCells '[] '[] where
    addInterCells Empty = return Empty

instance {-# OVERLAPPING #-} AddInterCells xs xs' =>
    AddInterCells (x ': xs) (x ': xs') where
        addInterCells (Ext x xs) = Ext x <$> addInterCells xs

instance {-# OVERLAPPABLE #-} (AddInterCells xs xs', Default t) =>
    AddInterCells xs (InterCell s t ': xs') where
        addInterCells xs = do
            cell <- InterCell @s @t <$> newIORef def
            cells <- addInterCells xs
            return $ Ext cell cells

type family WithoutInters (xs :: [*]) :: [*] where
    WithoutInters '[] = '[]
    WithoutInters (InterCell _ _ ': xs) = WithoutInters xs
    WithoutInters (x ': xs) = x ': WithoutInters xs

type family GetEnvFromMems (xs :: [*]) :: [*] where
    GetEnvFromMems '[] = '[]
    GetEnvFromMems (Memory _ '(rs, ws) _ ': xs) =
        Union rs (Union ws (GetEnvFromMems xs))