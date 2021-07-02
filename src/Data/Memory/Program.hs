-- This module defines Program and ParallelProgram, which group memory
-- computations and the environment they run in, as well as resolving their
-- dependencies at type level.

{-# LANGUAGE FunctionalDependencies, RecordWildCards, FlexibleContexts #-}

module Data.Memory.Program (
    ParallelProgram, Program, makeProgram, makeParallelProgram,
    runProgram, runProgramPartial, runParallelProgram, runParallelProgramPartial
) where

import Data.Memory.Types (Set, Memory, CellUpdate, NoConflicts_) 
import Data.Memory.RunMemory (RunPartialMems(..), RunMems(..))
import Data.Memory.RunMemoryConc
import Data.Type.TSort (ordered, OrderedConstraints)
import Data.Type.ComponentSearch (toSortedComponents, SortedComponentsConstraints)
import Data.Type.HList (HList(..))
import Data.Memory.EnvUtil (AAC, AddAutoCells(addAutoCells))
import Data.Memory.Dependencies (IsLessThan, NoOutputDep)

data Prog m e = Prog {
    mems :: HList m,
    env :: Set e
}

newtype ParallelProgram m e = ParallelProgram (Prog m e)
newtype Program m e = Program (Prog m e)

makeProgram :: (Monad m, OrderedConstraints IsLessThan mems mems',
    NoConflicts_ env, NoOutputDep mems) =>
    HList mems -> Set env -> m (Program mems' env)
makeProgram mems env = do
    let mems' = ordered @IsLessThan mems
    return $ Program $ Prog {mems = mems', ..}

makeProgramInters :: (OrderedConstraints IsLessThan mems mems',
    AAC env env' mems, NoConflicts_ env', NoOutputDep mems) =>
    HList mems -> Set env -> IO (Program mems' env')
makeProgramInters mems en = do
    let mems' = ordered @IsLessThan mems
    env <- addAutoCells en
    return $ Program $ Prog {mems = mems', ..}

makeParallelProgram :: (SortedComponentsConstraints IsLessThan mems mems',
    AAC env env' mems, NoConflicts_ env', NoOutputDep mems) =>
    HList mems -> Set env -> IO (ParallelProgram mems' env')
makeParallelProgram mems en = do
    let mems'' = toSortedComponents @IsLessThan mems
    env <- addAutoCells en
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