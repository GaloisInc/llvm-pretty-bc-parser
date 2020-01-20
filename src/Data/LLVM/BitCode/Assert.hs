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
  , ptrTo
  ) where

import           Control.Monad (MonadPlus, mplus)
import           Control.Monad (when)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Data.LLVM.BitCode.Record (Record)
import qualified Data.LLVM.BitCode.Record as Record
import           Text.LLVM.AST (Type', Typed, Ident)
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

-- | Assert that this thing is a pointer, get the underlying type
elimPtrTo :: (MonadFail m, MonadPlus m) => String -> Type' Ident -> m (Type' Ident)
elimPtrTo msg ptrTy = AST.elimPtrTo ptrTy `mplus`
                        (fail $ unlines [ msg
                                        , "Expected pointer type, found:"
                                        , show ptrTy
                                        ])

-- | Assert that this thing is a pointer
elimPtrTo_ :: (MonadFail m, MonadPlus m) => String -> Type' Ident -> m ()
elimPtrTo_ msg ptrTy = elimPtrTo msg ptrTy >> pure ()

-- | Assert that the first thing is a pointer to something of the type of the
-- second thing, e.g. in a load/store instruction.
--
-- See: https://github.com/llvm-mirror/llvm/blob/release_60/lib/Bitcode/Reader/BitcodeReader.cpp#L3328
ptrTo :: (MonadFail m, Show a, Show b)
      => String
      -> Typed a -- ^ The pointer
      -> Typed b -- ^ The value
      -> m ()
ptrTo sig ptr val = do
  case AST.typedType ptr of
    AST.PtrTo ptrTo_ ->
      when (AST.typedType val /= ptrTo_) $ fail $ unlines
        [ unwords [ "Expected first value to be a pointer to some type <ty>, and"
                  , "for the second value to be a value of type <ty>."
                  ]
        , "Instruction signature: " ++ sig
        , "Pointer type:  " ++ show (AST.typedType ptr)
        , "Value type:    " ++ show (AST.typedType val)
        , "Pointer value: " ++ show (AST.typedValue ptr)
        , "Value value:   " ++ show (AST.typedValue val)
        ]
    ty ->
      fail $ unlines $
        [ "Instruction expected a pointer argument."
        , "Instruction signature: " ++ sig
        , "Argument type: " ++ show ty
        ]
