module Test.Specialist.UnitTests.T3 where

import GHC.Specialist
import Test.Specialist.Types
import Test.Specialist.Utils

import System.Directory
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

specT3 :: TestSpec
specT3 =
    ExeTestSpec
      { exeTestSpecGetNotes = getNotesT3
      , exeTestSpecTest = testT3
      }

getNotesT3 :: IO [SpecialistNote]
getNotesT3 = do
    callCommand "test-T3 +RTS -l-au -RTS"
    specialistNotesFromEventLogFile "test-T3.eventlog" >>= \case
      Right notes -> do
        removeFile "test-T3.eventlog"
        return notes
      Left  _ ->
        assertFailure "failed to read specialist notes from test-T3.eventlog"

testT3 :: [SpecialistNote] -> TestTree
testT3 notes = testCase "T3" $ do
    length notes == 7 @?
      "expect exactly seven overloaded calls"
    mapM_ checkDictCount $ zip notes [1..]
    mapM_ checkBasicSuperclasses notes
  where
    checkDictCount :: (SpecialistNote, Int) -> Assertion
    checkDictCount (n, c) =
      length (specialistNoteDictInfos n) == c @?
        "expect " ++ show c ++ " dictionaries in the overloaded call"
