module Main where

import GHC.Stack.Types
import GHC.Stack.CCS

import Control.Exception

main :: IO ()
main = do
  f 100 `catch` \(SomeException _) -> print True

{-# NOINLINE f #-}
f :: (HasCallStack, Show a) => a -> IO ()
f x = currentCallStack >>= error . show . (, x)
