{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances,
    ScopedTypeVariables, RecordWildCards #-}

module Data.Memory.Program (
    ParallelProgram, Program, makeProgram, makeParallelProgram,
    runProgram, runProgramPartial, runParallelProgram, runParallelProgramPartial
) where

import Data.Memory.Types (Set, Memory, CellUpdate) 
import Data.Memory.RunMemory
import Data.Memory.RunMemoryConc
import Data.Type.TSort (ordered, OrderedConstraints)
import Data.Type.ComponentSearch (toSortedComponents, SortedComponentsConstraints)
import Data.Type.HList (HList(..), RearrangeList)

import MonadVar (MonadNew(new))
import Data.Default (Default(..))

data Prog m e l = Prog {
    mems :: HList m,
    env :: Set e,
    locals :: HList l
}

newtype ParallelProgram m e l = ParallelProgram (Prog m e l)
newtype Program m e l = Program (Prog m e l)

class MakeLocals mems m locals | mems -> locals where
    makeLocals :: HList mems -> m (HList locals)

instance Monad m => MakeLocals '[] m '[] where
    makeLocals _ = return HNil

instance (Monad m, MakeLocals xs m ls, MakeLocals xss m lss) =>
    MakeLocals (HList xs ': xss) m (HList ls ': lss) where
        makeLocals (mems :+: memss) = do
            rs <- makeLocals mems
            rss <- makeLocals memss
            return (rs :+: rss)

instance (Monad m, MonadNew m v, MakeLocals xs m ls, Default a) =>
    MakeLocals (Memory m (v a) cs b ': xs) m (v a ': ls) where
        makeLocals (mem :+: mems) = do
            r <- new def
            rs <- makeLocals mems
            return (r :+: rs)

instance (Monad m, MakeLocals xs m ls) =>
    MakeLocals (Memory m () cs b ': xs) m (() ': ls) where
        makeLocals (mem :+: mems) = do
            rs <- makeLocals mems
            return (() :+: rs)

makeProgram :: (Monad m, MakeLocals mems' m locals,
    OrderedConstraints mems mems') =>
    HList mems -> Set env -> m (Program mems' env locals)
makeProgram mems env = do
    let mems' = ordered mems
    locals <- makeLocals mems'
    return $ Program $ Prog {mems = mems', ..}

makeParallelProgram :: (Monad m, MakeLocals mems'' m locals,
    SortedComponentsConstraints mems mems' mems'') =>
    HList mems -> Set env -> m (ParallelProgram mems'' env locals)
makeParallelProgram mems env = do
    let mems'' = toSortedComponents mems
    locals <- makeLocals mems''
    return $ ParallelProgram $ Prog {mems = mems'', ..}

runProgram :: (RunMems m xs env locals out) =>
    Program xs env locals -> m (HList out)
runProgram (Program Prog {..}) = runMems mems env locals

runProgramPartial :: (RunPartialMems m xs env locals) =>
    Program xs env locals -> [CellUpdate] -> m () -> m ()
runProgramPartial (Program Prog {..}) = runPartialMems mems env locals

runParallelProgram :: (RunMultiMems xs env locals) =>
    ParallelProgram xs env locals -> IO ()
runParallelProgram (ParallelProgram Prog {..}) = runMultiMems mems env locals

runParallelProgramPartial :: (RunMultiPartialMems xs env locals) =>
    ParallelProgram xs env locals -> [CellUpdate] -> IO () -> IO ()
runParallelProgramPartial (ParallelProgram Prog {..}) =
    runMultiPartialMems mems env locals