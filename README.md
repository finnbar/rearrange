# rearrange

Haskell code for compile-time detection and resolution of data-flow dependencies.
If you're looking for an example, look at Main.hs, Robot.hs and ComplexExample.hs.
To run both examples, just use `stack run`.

Code corresponding to Section 2 can be found in:
* Data.Memory.Types (various type definitions)
* Data.Memory.Memory (the Memory graded monad definition)
* Data.Memory.MemoryCell (readCell/writeCell)
* MonadRW (MonadRW and Constr)
* Data.Type.HList (the HList definition)

Code corresponding to Sections 3 and 4 can be found in:
* Data.Type.AdjacencyList (type-level adjacency list)
* Data.Type.GraphUtils (DFS, connected components search)
* Data.Type.TSort (a wrapper around DFS for topological sort, as well as the value-level definition)
* Data.Type.ComponentSearch (like TSort, but for component search)

Code corresponding to Section 5 can be found in:
* Data.Type.Dependencies (IsLessThan, which finds data flow dependencies between memory computations)
* Data.Memory.RunMemory (runs Memory computations serially)
* Data.Memory.RunMemoryConc (runs Memory computations concurrently)
* Data.Memory.ToCells (functions for building Cells and thus environments)
* Data.Memory.Program (functions for building and running Programs)
* Data.Memory.EnvUtil (functions for aiding type inference in Section 5.6)