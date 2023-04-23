{- |
Module      : Data.LLVM.BitCode.Assert
Description : This module implements exceptions and warnings about bitcode.
License     : BSD3
Maintainer  : lbarrett
Stability   : experimental

This module is meant to be imported qualified as @Assert@

-}

{-# LANGUAGE CPP #-}
module Data.LLVM.BitCode.Assert
  ( failWithMsg
  , unknownEntity
  -- ** Record size
  , recordSizeLess
  , recordSizeGreater
  , recordSizeBetween
  , recordSizeIn

  -- ** Types
  , elimPtrTo
  , elimPtrTo_
  ) where

import           Control.Monad (MonadPlus, mplus)
import           Control.Monad (when)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Data.LLVM.BitCode.Record (Record)
import qualified Data.LLVM.BitCode.Record as Record
import           Text.LLVM.AST (Type', Ident)
import qualified Text.LLVM.AST as AST

supportedCompilerMessage :: [String]
supportedCompilerMessage =
  [ "Are you sure you're using a supported compiler?"
  , "Check here: https://github.com/GaloisInc/llvm-pretty-bc-parser"
  ]

-- | Call 'fail' with a helpful hint to the user
failWithMsg :: MonadFail m => String -> m a
failWithMsg s = fail $ unlines (s:supportedCompilerMessage)

-- | For when an unknown value of an enumeration is encountered
unknownEntity :: (MonadFail m, Show a) => String -> a -> m b
unknownEntity sort val = failWithMsg ("Unknown " ++ sort ++ " " ++ show val)

----------------------------------------------------------------
-- ** Record sizes

recordSizeCmp :: MonadFail m => String -> (Int -> Bool) -> Record -> m ()
recordSizeCmp msg compare_ record =
  let len = length (Record.recordFields record)
  in when (compare_ len) $ failWithMsg $ unlines $
       [ "Invalid record size: " ++ show len, msg ]

recordSizeLess :: MonadFail m => Record -> Int -> m ()
recordSizeLess r i = recordSizeCmp "Expected size less than" (i <=) r

recordSizeGreater :: MonadFail m => Record -> Int -> m ()
recordSizeGreater r i = recordSizeCmp "Expected size greater than" (<= i) r

recordSizeBetween :: MonadFail m => Record -> Int -> Int -> m ()
recordSizeBetween record lb ub =
  recordSizeGreater record lb >> recordSizeLess record ub

recordSizeIn :: MonadFail m => Record -> [Int] -> m ()
recordSizeIn record ns =
  let len = length (Record.recordFields record)
  in when (not (len `elem` ns)) $ failWithMsg $ unlines $
       [ "Invalid record size: " ++ show len
       , "Expected one of: " ++ show ns
       ]


----------------------------------------------------------------
-- ** Types

-- | Assert that this thing is a @'PtrTo' ty@ and return the underlying @ty@.
--
-- Think carefully before using this function, as it will not work as you would
-- expect when the type is an opaque pointer.
-- See @Note [Pointers and pointee types]@.
elimPtrTo :: (MonadFail m, MonadPlus m) => String -> Type' Ident -> m (Type' Ident)
elimPtrTo msg ptrTy = AST.elimPtrTo ptrTy `mplus`
                        (fail $ unlines [ msg
                                        , "Expected pointer type, found:"
                                        , show ptrTy
                                        ])

-- | Assert that this thing is a 'PtrTo' type.
--
-- Think carefully before using this function, as it will not work as you would
-- expect when the type is an opaque pointer.
-- See @Note [Pointers and pointee types]@.
elimPtrTo_ :: (MonadFail m, MonadPlus m) => String -> Type' Ident -> m ()
elimPtrTo_ msg ptrTy = elimPtrTo msg ptrTy >> pure ()

{-
Note [Pointers and pointee types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike LLVM itself, llvm-pretty and llvm-pretty-bc-parser allow mixing opaque
and non-opaque pointers. A consequence of this is that we generally avoid
pattern matching on PtrTo (non-opaque pointer) types and inspecting the
underlying pointee types. This sort of code simply won't work for PtrOpaque
types, which lack pointee types.

The elimPtrTo and elimPtrTo_ functions go against this rule, as they retrieve
the pointee type in a PtrTo. These functions are primarily used for supporting
old versions of LLVM which do not store the necessary type information in the
instruction itself.
-}
