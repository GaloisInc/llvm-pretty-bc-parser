{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}

module Factorial where

import Text.LLVM
import Text.LLVM.AST

factorial :: Module
factorial  = snd $ runLLVM $ do
  fact <- define emptyFunAttrs (iT 32) "factorial" (iT 32) $ \ x -> do
    "entry"
    jump "test"

    rec "test"
        i   <- phi (iT 32) [x     `from` "entry", i'   `from` "incr"]
        acc <- phi (iT 32) [int 1 `from` "entry", acc' `from` "incr"]

        b   <- icmp Iule i (int 1)
        br b "exit" "incr"

        "incr"
        acc' <- mul acc i
        i'   <- sub i (int 1)
        jump "test"

    "exit"
    ret acc


  define emptyFunAttrs voidT "test" () $ do
    call fact [iT 32 -: int 10]
    retVoid
