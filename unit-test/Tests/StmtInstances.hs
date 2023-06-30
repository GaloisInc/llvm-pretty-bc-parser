module Tests.StmtInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Text.LLVM.AST

import Tests.ExpressionInstances ()


instance Arbitrary AtomicRWOp where arbitrary = genericArbitrary uniform
instance Arbitrary AtomicOrdering where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Instr' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Clause' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Stmt' lab) where arbitrary = genericArbitrary uniform
