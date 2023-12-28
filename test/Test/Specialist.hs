module Test.Specialist where

import Test.Specialist.Types
import Test.Specialist.UnitTests.T1

import Test.Tasty

testSpecs :: [TestSpec]
testSpecs =
    [ specT1
    ]

interpretTestSpec :: TestSpec -> IO TestTree
interpretTestSpec = \case
    ExeTestSpec{..} -> exeTestSpecTest <$> exeTestSpecGetNotes