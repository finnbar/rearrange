{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Dependencies (
    IsLessThan,
    PartialOrder,
    GetDependency
    ) where

import Data.Memory (Memory(..))
import Data.Type.Utils (NonEmptyIntersect)

import GHC.TypeLits
import Fcf

data PartialOrder = DLT | DGT | NOT

data IsLessThan :: * -> * -> Exp Bool
type instance Eval (IsLessThan x y) = Eval (Pure (PartialToBool (GetDependency x y)))

type family PartialToBool (x :: PartialOrder) :: Bool where
    PartialToBool 'DLT = 'True
    PartialToBool 'DGT = 'False
    PartialToBool 'NOT = 'False

type family GetDependency x y :: PartialOrder where
    GetDependency (Memory rws _) (Memory rws' _) = GetTupleDependency rws rws'
    GetDependency (Memory rws a) (t b) = GetDependency (Memory rws a) b
    GetDependency (t a) (Memory rws b) = GetDependency a (Memory rws b)
    GetDependency (t a) (t' b) = GetDependency a b
    GetDependency x y =
        TypeError (Text "Cannot get dependency of non-Memory types: " :<>:
            ShowType x :<>: Text " and " :<>: ShowType y :<>: Text ".")

type family DependencyLoopError rs ws rs' ws' where
    DependencyLoopError rs ws rs' ws' = TypeError
        (Text "Circular dependency between Memory defined by read effects "
        :<>: ShowType rs :<>: Text " and write effects " :<>: ShowType ws :$$:
        Text "and MMSF defined by read effects " :<>: ShowType rs' :<>: Text " and write effects "
        :<>: ShowType ws' :<>: Text "." :$$: Text "You can't define funtions with circular dependencies!"
        :$$: Text "To fix this, remove read effects or write effects to break the cycle.")
    
type family GetTupleDependency (x :: ([*], [*])) (y :: ([*], [*])) :: PartialOrder where
    GetTupleDependency '(rs, ws) '(rs', ws') =
        If (NonEmptyIntersect rs ws')
            (If (NonEmptyIntersect ws rs') (DependencyLoopError rs ws rs' ws') 'DGT)
            (If (NonEmptyIntersect ws rs') 'DLT 'NOT)