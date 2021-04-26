{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ConstraintKinds,
    FlexibleInstances #-}

module MonadRW (
    MonadRW(..)
) where

import Data.Kind (Constraint)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(poke, peek))
import GHC.Conc
    (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Concurrent.MVar (MVar, putMVar, readMVar)
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.ST (ST)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Control.Monad.IO.Class (MonadIO(..), liftIO)

-- This module is for MonadRW, a generalisation of MonadRead and MonadWrite to
-- allow for constraints. It would have been nice to provide an instance that
-- just takes the relevant instances from MonadRead and MonadWrite, but that
-- has issues with functional dependences.

class Unconstrained a
instance Unconstrained a

class MonadRW (m :: * -> *) (v :: * -> *) (c :: * -> Constraint) | m v -> c where
    readVar :: c a => v a -> m a
    writeVar :: c a => v a -> a -> m ()

instance (MonadIO m) => MonadRW m Ptr Storable where
    readVar = liftIO . peek
    writeVar v = liftIO . poke v

instance (MonadIO m) => MonadRW m TVar Unconstrained where
    readVar = liftIO . readTVarIO
    writeVar v = liftIO . atomically . writeTVar v

instance MonadRW STM TVar Unconstrained where
    readVar = readTVar
    writeVar = writeTVar

instance (MonadIO m) => MonadRW m MVar Unconstrained where
    readVar = liftIO . readMVar
    writeVar v = liftIO . putMVar v

instance (MonadIO m) => MonadRW m IORef Unconstrained where
    readVar = liftIO . readIORef
    writeVar v = liftIO . writeIORef v

instance MonadRW (ST s) (STRef s) Unconstrained where
    readVar = readSTRef
    writeVar = writeSTRef