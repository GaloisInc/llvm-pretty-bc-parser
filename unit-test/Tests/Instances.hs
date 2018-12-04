-- | Arbitrary instances for LLVM AST nodes, for QuickCheck.
--
-- This module takes a long time to compile... small price to pay for not
-- writing instances.
module Tests.Instances where

import Generic.Random hiding ((%))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Data.LLVM.Internal
import Text.LLVM.AST

-------------------------------------------------------------------------
-- ** llvm-pretty

instance Arbitrary Module                                             where arbitrary = genericArbitrary uniform
instance Arbitrary NamedMd                                            where arbitrary = genericArbitrary uniform
instance Arbitrary UnnamedMd                                          where arbitrary = genericArbitrary uniform
instance Arbitrary GlobalAlias                                        where arbitrary = genericArbitrary uniform
instance Arbitrary LayoutSpec                                         where arbitrary = genericArbitrary uniform
instance Arbitrary Mangling                                           where arbitrary = genericArbitrary uniform
instance Arbitrary SelectionKind                                      where arbitrary = genericArbitrary uniform
instance Arbitrary PrimType                                           where arbitrary = genericArbitrary uniform
instance Arbitrary FloatType                                          where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Type' lab)                       where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (NullResult lab)                  where arbitrary = genericArbitrary uniform
instance Arbitrary TypeDecl                                           where arbitrary = genericArbitrary uniform
instance Arbitrary Global                                             where arbitrary = genericArbitrary uniform
instance Arbitrary GlobalAttrs                                        where arbitrary = genericArbitrary uniform
instance Arbitrary Declare                                            where arbitrary = genericArbitrary uniform
instance Arbitrary Define                                             where arbitrary = genericArbitrary uniform
instance Arbitrary FunAttr                                            where arbitrary = genericArbitrary uniform
instance Arbitrary BlockLabel                                         where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (BasicBlock' lab)                 where arbitrary = genericArbitrary uniform
instance Arbitrary Linkage                                            where arbitrary = genericArbitrary uniform
instance Arbitrary Visibility                                         where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Typed lab)                       where arbitrary = genericArbitrary uniform
instance Arbitrary ArithOp                                            where arbitrary = genericArbitrary uniform
instance Arbitrary BitOp                                              where arbitrary = genericArbitrary uniform
instance Arbitrary ConvOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary AtomicRWOp                                         where arbitrary = genericArbitrary uniform
instance Arbitrary AtomicOrdering                                     where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Instr' lab)                      where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Clause' lab)                     where arbitrary = genericArbitrary uniform
instance Arbitrary ICmpOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary FCmpOp                                             where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Value' lab)                      where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (ValMd' lab)                      where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DebugLoc' lab)                   where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (Stmt' lab)                       where arbitrary = genericArbitrary uniform
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
instance Arbitrary DISubrange                                         where arbitrary = genericArbitrary uniform
instance Arbitrary lab => Arbitrary (DISubroutineType' lab)           where arbitrary = genericArbitrary uniform

-- Newtypes
instance Arbitrary Ident  where arbitrary = genericArbitrary uniform
instance Arbitrary Symbol where arbitrary = genericArbitrary uniform
instance Arbitrary GC     where arbitrary = genericArbitrary uniform

-------------------------------------------------------------------------
-- ** llvm-pretty-bc-parser

instance Arbitrary PartialUnnamedMd where arbitrary = genericArbitrary uniform
