{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import           Data.TreeDiff
import qualified Text.LLVM.AST as AST
import           Text.LLVM.Triple as Triple

deriving instance ToExpr (AST.Type' AST.Ident)
deriving instance ToExpr (AST.Typed AST.Ident)
deriving instance ToExpr (AST.Typed AST.Value)
deriving instance ToExpr AST.Alignment
deriving instance ToExpr AST.ArithOp
deriving instance ToExpr AST.AtomicOrdering
deriving instance ToExpr AST.AtomicRWOp
deriving instance ToExpr AST.BasicBlock
deriving instance ToExpr AST.BitOp
deriving instance ToExpr AST.BlockLabel
deriving instance ToExpr AST.Clause
deriving instance ToExpr AST.ConstExpr
deriving instance ToExpr AST.ConvOp
deriving instance ToExpr AST.DIArgList
deriving instance ToExpr AST.DIBasicType
deriving instance ToExpr AST.DICompileUnit
deriving instance ToExpr AST.DICompositeType
deriving instance ToExpr AST.DIDerivedType
deriving instance ToExpr AST.DIExpression
deriving instance ToExpr AST.DIFile
deriving instance ToExpr AST.DIGlobalVariable
deriving instance ToExpr AST.DIGlobalVariableExpression
deriving instance ToExpr AST.DIImportedEntity
deriving instance ToExpr AST.DILabel
deriving instance ToExpr AST.DILexicalBlock
deriving instance ToExpr AST.DILexicalBlockFile
deriving instance ToExpr AST.DILocalVariable
deriving instance ToExpr AST.DINameSpace
deriving instance ToExpr AST.DISubprogram
deriving instance ToExpr AST.DISubrange
deriving instance ToExpr AST.DISubroutineType
deriving instance ToExpr AST.DITemplateTypeParameter
deriving instance ToExpr AST.DITemplateValueParameter
deriving instance ToExpr AST.DbgRecAssign
deriving instance ToExpr AST.DbgRecDeclare
deriving instance ToExpr AST.DbgRecLabel
deriving instance ToExpr AST.DbgRecValue
deriving instance ToExpr AST.DbgRecValueSimple
deriving instance ToExpr AST.DebugInfo
deriving instance ToExpr AST.DebugLoc
deriving instance ToExpr AST.DebugRecord
deriving instance ToExpr AST.Declare
deriving instance ToExpr AST.Define
deriving instance ToExpr AST.FCmpOp
deriving instance ToExpr AST.FP80Value
deriving instance ToExpr AST.FloatType
deriving instance ToExpr AST.FunAttr
deriving instance ToExpr AST.FunctionPointerAlignType
deriving instance ToExpr AST.GC
deriving instance ToExpr AST.GEPAttr
deriving instance ToExpr AST.Global
deriving instance ToExpr AST.GlobalAlias
deriving instance ToExpr AST.GlobalAttrs
deriving instance ToExpr AST.ICmpOp
deriving instance ToExpr AST.Ident
deriving instance ToExpr AST.Instr
deriving instance ToExpr AST.LayoutSpec
deriving instance ToExpr AST.Linkage
deriving instance ToExpr AST.Mangling
deriving instance ToExpr AST.Module
deriving instance ToExpr AST.NamedMd
deriving instance ToExpr AST.PointerSize
deriving instance ToExpr AST.PrimType
deriving instance ToExpr AST.RangeSpec
deriving instance ToExpr AST.SelectionKind
deriving instance ToExpr AST.Stmt
deriving instance ToExpr AST.Storage
deriving instance ToExpr AST.Symbol
deriving instance ToExpr AST.TypeDecl
deriving instance ToExpr AST.UnaryArithOp
deriving instance ToExpr AST.UnnamedMd
deriving instance ToExpr AST.ValMd
deriving instance ToExpr AST.Value
deriving instance ToExpr AST.Visibility
deriving instance ToExpr Arch
deriving instance ToExpr Environment
deriving instance ToExpr OS
deriving instance ToExpr ObjectFormat
deriving instance ToExpr SubArch
deriving instance ToExpr Triple.TargetTriple
deriving instance ToExpr Vendor


-- deriving instance ToExpr AST.DwarfAttrEncoding
