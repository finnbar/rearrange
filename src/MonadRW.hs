-- This module is for MonadRW, a generalisation of MonadRead and MonadWrite to
-- allow for constraints.

{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleInstances #-}

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

class MonadRW (m :: * -> *) (v :: * -> *) where
    type Constr m v a :: Constraint
    readVar :: Constr m v a => v a -> m a
    writeVar :: Constr m v a => v a -> a -> m ()

instance (MonadIO m) => MonadRW m Ptr where
    type Constr m Ptr a = Storable a
    readVar = liftIO . peek
    writeVar v = liftIO . poke v

instance (MonadIO m) => MonadRW m TVar where
    type Constr m TVar a = ()
    readVar = liftIO . readTVarIO
    writeVar v = liftIO . atomically . writeTVar v

instance MonadRW STM TVar where
    type Constr STM TVar a = ()
    readVar = readTVar
    writeVar = writeTVar

instance (MonadIO m) => MonadRW m MVar where
    type Constr m MVar a = ()
    readVar = liftIO . readMVar
    writeVar v = liftIO . putMVar v

instance (MonadIO m) => MonadRW m IORef where
    type Constr m IORef a = ()
    readVar = liftIO . readIORef
    writeVar v = liftIO . writeIORef v

instance MonadRW (ST s) (STRef s) where
    type Constr (ST s) (STRef s) a = ()
    readVar = readSTRef
    writeVar = writeSTRef