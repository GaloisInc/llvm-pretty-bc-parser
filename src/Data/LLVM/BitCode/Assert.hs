{- |
Module      : Data.LLVM.BitCode.Assert
Description : This module implements exceptions and warnings about bitcode.
License     : BSD3
Maintainer  : lbarrett
Stability   : experimental

This module is meant to be imported qualified as @Assert@

-}

module Data.LLVM.BitCode.Assert
  ( failWithMsg
  , unknownEntity
  , recordSizeLess
  , recordSizeGreater
  , recordSizeBetween
  , recordSizeIn
  ) where

import qualified Data.LLVM.BitCode.Record as Record
import           Data.LLVM.BitCode.Record (Record)
import           Control.Monad (when)
import           Control.Monad.Fail (MonadFail)

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
