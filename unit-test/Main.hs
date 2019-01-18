module Main (main) where

import qualified Test.Tasty as T
import qualified Tests.Metadata

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ Tests.Metadata.tests
                                           ]
