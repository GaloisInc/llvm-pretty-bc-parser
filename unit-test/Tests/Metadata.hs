module Tests.Metadata (tests) where

import qualified Data.Sequence as Seq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.LLVM.Internal
import Text.LLVM.AST

import Tests.Instances()

tests :: TestTree
tests = testGroup "Metadata"
    [ testCase "test dedup metadata" $
        mempty @?= dedupMetadata mempty

    -- The first two should remain unchanged, but the third duplicates information in
    -- the second, so should be updated to hold a reference to it.
    , testGroup "test dedup metadata" $
        let mkPum i v = PartialUnnamedMd i v True
            val1 = mkPum 1 (ValMdString "str")
            val2 = mkPum 2 (ValMdDebugInfo (DebugInfoExpression (DIExpression [])))
            val3 = mkPum 3 (ValMdNode [Just (pumValues val2)])
            deduped = dedupMetadata (Seq.fromList [val1, val2, val3])
        in [ testCase "1" $
               Just val1 @?= deduped Seq.!? 0
           , testCase "2" $
               Just val2 @?= deduped Seq.!? 1
           , testCase "3" $
               Just (mkPum 3 (ValMdNode [Just (ValMdRef 2)])) @?=
                  deduped Seq.!? 2
           ]

    -- Deduplication should not changes: references or strings
    -- This test takes too long.
    -- , testProperty "dedup metadata" $
    --     \lst i -> i >= 0 && length lst > i ==>
    --       let result = dedupMetadata lst
    --       in if lst !! i /= result !! i
    --          then case pumValues (result !! i) of
    --                 ValMdRef _    -> False
    --                 ValMdString _ -> False
    --                 _             -> True
    --          else True
    ]
