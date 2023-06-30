module Tests.PrimInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Text.LLVM.AST


instance Arbitrary PrimType where arbitrary = genericArbitrary uniform
instance Arbitrary FloatType where arbitrary = genericArbitrary uniform
instance Arbitrary FP80Value where arbitrary = genericArbitrary uniform
instance Arbitrary Ident  where arbitrary = genericArbitrary uniform
instance Arbitrary Symbol where arbitrary = genericArbitrary uniform
