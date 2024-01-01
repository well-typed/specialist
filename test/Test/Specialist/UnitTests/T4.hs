module Test.Specialist.UnitTests.T4 where

import GHC.Specialist
import Test.Specialist.Types
import Test.Specialist.Utils

import System.Directory
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

specT4 :: TestSpec
specT4 =
    ExeTestSpec
      { exeTestSpecGetNotes = getNotesT4
      , exeTestSpecTest = testT4
      }

getNotesT4 :: IO [SpecialistNote]
getNotesT4 = do
    callCommand "test-T4 +RTS -l-au -RTS"
    specialistNotesFromEventLogFile "test-T4.eventlog" >>= \case
      Right notes -> do
        removeFile "test-T4.eventlog"
        return notes
      Left  _ ->
        assertFailure "failed to read specialist notes from test-T4.eventlog"

testT4 :: [SpecialistNote] -> TestTree
testT4 notes = testCase "T4" $ do
    length notes == 1 @?
      "expect exactly seven overloaded calls"
    mapM_
      ( checkSuperclasses
          [ ("C:C2", "C:Ord")
          , ("C:C2", "C:C1")
          ]
      )
      notes
