module Tests.ExpressionInstances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Text.LLVM.AST

import Tests.PrimInstances ()


instance Arbitrary Alignment where arbitrary = genericArbitrary uniform
instance Arbitrary PointerSize where arbitrary = genericArbitrary uniform
instance Arbitrary Storage where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Type' lab)                       where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Typed lab)                       where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (ValMd' lab)                      where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Value' lab)                      where arbitrary = genericArbitrary uniform
instance Arbitrary ArithOp                                            where arbitrary = genericArbitrary uniform
instance Arbitrary UnaryArithOp                                       where arbitrary = genericArbitrary uniform
instance Arbitrary BitOp                                              where arbitrary = genericArbitrary uniform
instance Arbitrary ConvOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary ICmpOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary FCmpOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DebugLoc' lab)                   where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (ConstExpr' lab)                  where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DebugInfo' lab)                  where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DIImportedEntity' lab)           where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DITemplateTypeParameter' lab)    where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DITemplateValueParameter' lab)   where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DINameSpace' lab)                where arbitrary = genericArbitrary uniform
instance Arbitrary DIBasicType                                        where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DICompileUnit' lab)              where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DICompositeType' lab)            where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DIDerivedType' lab)              where arbitrary = genericArbitrary uniform
instance Arbitrary DIExpression                                       where arbitrary = genericArbitrary uniform
instance Arbitrary DIFile                                             where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DIGlobalVariable' lab)           where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DIGlobalVariableExpression' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DILexicalBlock' lab)             where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DILexicalBlockFile' lab)         where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DILocalVariable' lab)            where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DISubprogram' lab)               where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DISubrange' lab)                 where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DISubroutineType' lab)           where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DILabel' lab)                    where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DIArgList' lab)                  where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DebugRecord' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DbgRecValue' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DbgRecValueSimple' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DbgRecDeclare' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DbgRecAssign' lab) where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DbgRecLabel' lab) where arbitrary = genericArbitrary uniform
