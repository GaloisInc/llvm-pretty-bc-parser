module Tests.PrimInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Text.LLVM.AST

-- FUTURE: for floating point values, given what we're doing here, it
-- might be worth biasing the generation. Also if there's a way to do
-- so, generating the known special cases (+/-0, +/-Inf, and a few
-- NaNs and subnormals) up front before randomizing.

instance Arbitrary PrimType where arbitrary = genericArbitrary uniform
instance Arbitrary FloatType where arbitrary = genericArbitrary uniform
instance Arbitrary FPHalfValue where arbitrary = genericArbitrary uniform
instance Arbitrary FPBFloatValue where arbitrary = genericArbitrary uniform
instance Arbitrary FP80Value where arbitrary = genericArbitrary uniform
instance Arbitrary FP128Value where arbitrary = genericArbitrary uniform
instance Arbitrary FP128_PPCValue where arbitrary = genericArbitrary uniform
instance Arbitrary Ident  where arbitrary = genericArbitrary uniform
instance Arbitrary Symbol where arbitrary = genericArbitrary uniform
