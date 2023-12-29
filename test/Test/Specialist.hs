module Test.Specialist where

import Test.Specialist.Types
import Test.Specialist.UnitTests.T1
import Test.Specialist.UnitTests.T2
import Test.Specialist.UnitTests.T3

import Test.Tasty

testSpecs :: [TestSpec]
testSpecs =
    [ specT1
    , specT2
    , specT3
    ]

interpretTestSpec :: TestSpec -> IO TestTree
interpretTestSpec = \case
    ExeTestSpec{..} ->
      exeTestSpecTest <$> exeTestSpecGetNotes
    ShouldCompileTestSpec{..} ->
      return shouldCompileTestSpecExe
