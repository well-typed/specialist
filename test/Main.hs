module Main where

import Test.Specialist

import Test.Tasty

main :: IO ()
main = do
    testTrees <- mapM interpretTestSpec testSpecs
    defaultMain $ testGroup "specialist plugin tests" testTrees
