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

data Prog m e = Prog {
    mems :: HList m,
    env :: Set e
}

newtype ParallelProgram m e = ParallelProgram (Prog m e)
newtype Program m e = Program (Prog m e)

-- TODO: this is where we need to expand env
makeProgram :: (Monad m, OrderedConstraints mems mems') =>
    HList mems -> Set env -> m (Program mems' env)
makeProgram mems env = do
    let mems' = ordered mems
    return $ Program $ Prog {mems = mems', ..}

makeParallelProgram :: (Monad m, SortedComponentsConstraints mems mems' mems'') =>
    HList mems -> Set env -> m (ParallelProgram mems'' env)
makeParallelProgram mems env = do
    let mems'' = toSortedComponents mems
    return $ ParallelProgram $ Prog {mems = mems'', ..}

runProgram :: RunMems m xs env out =>
    Program xs env -> m (HList out)
runProgram (Program Prog {..}) = runMems mems env

runProgramPartial :: RunPartialMems m xs env =>
    Program xs env -> [CellUpdate] -> m ()
runProgramPartial (Program Prog {..}) = runPartialMems mems env

runParallelProgram :: RunMultiMems xs env =>
    ParallelProgram xs env -> IO ()
runParallelProgram (ParallelProgram Prog {..}) = runMultiMems mems env

runParallelProgramPartial :: RunMultiPartialMems xs env =>
    ParallelProgram xs env -> [CellUpdate] -> IO () -> IO ()
runParallelProgramPartial (ParallelProgram Prog {..}) =
    runMultiPartialMems mems env