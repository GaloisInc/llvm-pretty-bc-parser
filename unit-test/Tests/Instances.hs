-- | Arbitrary instances for LLVM AST nodes, for QuickCheck.
--
-- These declarations are actually spread across a number of files.  This spread
-- must allow for dependencies between instances, which would not be as much of
-- an issue if they were all collected in a single file, but the single file
-- takes much more time and memory to compile.
--
-- In one file:                                  2m13s compilation, ~10GB RAM
-- top + triple + prim + expr + stmt + funcdata:   32s compilation, ~ 1GB RAM

module Tests.Instances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Data.LLVM.Internal
import Text.LLVM.AST

import Tests.TripleInstances ()
import Tests.FuncDataInstances ()
import Tests.ExpressionInstances ()
import Tests.StmtInstances ()

-------------------------------------------------------------------------
-- ** llvm-pretty

instance Arbitrary Module where arbitrary = genericArbitrary uniform
instance Arbitrary NamedMd where arbitrary = genericArbitrary uniform
instance Arbitrary UnnamedMd where arbitrary = genericArbitrary uniform
instance Arbitrary GlobalAlias where arbitrary = genericArbitrary uniform
instance Arbitrary LayoutSpec where arbitrary = genericArbitrary uniform
instance Arbitrary FunctionPointerAlignType where arbitrary = genericArbitrary uniform
instance Arbitrary Mangling where arbitrary = genericArbitrary uniform
instance Arbitrary SelectionKind where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (NullResult lab) where arbitrary = genericArbitrary uniform

-------------------------------------------------------------------------
-- ** llvm-pretty-bc-parser

instance Arbitrary PartialUnnamedMd where arbitrary = genericArbitrary uniform
