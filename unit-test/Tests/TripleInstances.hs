module Tests.TripleInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import qualified Text.LLVM.Triple.AST as Triple


instance Arbitrary Triple.Arch where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.Environment where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.OS where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.ObjectFormat where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.SubArch where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.TargetTriple where arbitrary = genericArbitrary uniform
instance Arbitrary Triple.Vendor where arbitrary = genericArbitrary uniform
