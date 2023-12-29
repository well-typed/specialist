module Test.Specialist.UnitTests.T2 where

import Test.Specialist.Types

import System.Process
import Test.Tasty.HUnit

specT2 :: TestSpec
specT2 =
    ShouldCompileTestSpec
      { shouldCompileTestSpecExe = testCase "T2" $ callCommand "test-T2"
      }
