module Tests.FuncDataInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Text.LLVM.AST

import Tests.StmtInstances ()


instance Arbitrary BlockLabel where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (BasicBlock' lab) where
  arbitrary = genericArbitrary uniform

instance Arbitrary TypeDecl where arbitrary = genericArbitrary uniform
instance Arbitrary Global where arbitrary = genericArbitrary uniform
instance Arbitrary GlobalAttrs where arbitrary = genericArbitrary uniform
instance Arbitrary Linkage where arbitrary = genericArbitrary uniform
instance Arbitrary Visibility where arbitrary = genericArbitrary uniform
instance Arbitrary ThreadLocality where arbitrary = genericArbitrary uniform

instance Arbitrary FunAttr where arbitrary = genericArbitrary uniform
instance Arbitrary Define where arbitrary = genericArbitrary uniform
instance Arbitrary Declare where arbitrary = genericArbitrary uniform
instance Arbitrary GC     where arbitrary = genericArbitrary uniform
