{-# LANGUAGE FlexibleInstances, RankNTypes, ScopedTypeVariables,
    FlexibleContexts, UndecidableInstances #-}

module Data.Memory.ToCells where

import MonadRW

import Data.Memory.Types
import Data.Type.HList
import Foreign.Storable
import Foreign.Ptr
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.Type.Set hiding (Proxy)

toCell :: forall (s :: Symbol) t m v c. (Monad m, MonadRW m v, Constr m v t)
    => m (v t) -> m (Cell v s t)
toCell action = action >>= \ptr -> return (Cell @s @t @m ptr)

printCell :: forall v s t. (KnownSymbol s, MonadRW IO v, Constr IO v t, Show t)
    => Cell v s t -> IO ()
printCell (Cell p) = readVar p >>= \v ->
    putStrLn $ symbolVal (Proxy @s) ++ ": " ++ show v

class PrintCells xs where
    printCells :: Set xs -> IO ()

instance PrintCells '[] where
    printCells Empty = return ()

instance (PrintCells xs, KnownSymbol s, MonadRW IO v, Constr IO v t, Show t) =>
    PrintCells (Cell v s t ': xs) where
        printCells (Ext x xs) = printCell x >> printCells xs

toSet :: (Sortable s, Nubable (Sort s), ToSet s) =>
    HList s -> Set (Nub (Sort s))
toSet l = asSet $ toSet_ l

class ToSet xs where
    toSet_ :: HList xs -> Set xs

instance ToSet '[] where
    toSet_ _ = Empty

instance ToSet xs => ToSet (x ': xs) where
    toSet_ (x :+: xs) = Ext x (toSet_ xs)

type family Extracted (m :: * -> *) (xs :: [*]) :: [*] where
    Extracted m '[] = '[]
    Extracted m (m x ': xs) = x ': Extracted m xs

class Distribute xs m where
    distribute :: HList xs -> m (HList (Extracted m xs))

instance Monad m => Distribute '[] m where
    distribute _ = return HNil

instance (Distribute xs m, Monad m) => Distribute (m x ': xs) m where
    distribute (action :+: actions) = do
        res <- action
        ress <- distribute actions
        return $ res :+: ress
    {-# NOINLINE distribute #-}