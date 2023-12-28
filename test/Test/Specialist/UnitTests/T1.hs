module Test.Specialist.UnitTests.T1 where

import GHC.Specialist
import Test.Specialist.Types

import Data.List
import System.Directory
import System.Process
import GHC.InfoProv
import Test.Tasty
import Test.Tasty.HUnit

specT1 :: TestSpec
specT1 =
    ExeTestSpec
      { exeTestSpecGetNotes = getNotesT1
      , exeTestSpecTest = testT1
      }

getNotesT1 :: IO [SpecialistNote]
getNotesT1 = do
    callCommand "test-T1 +RTS -l-au -RTS"
    specialistNotesFromEventLogFile "test-T1.eventlog" >>= \case
      Right notes -> do
        removeFile "test-T1.eventlog"
        return notes
      Left  _ ->
        assertFailure "failed to read specialist notes from test-T1.eventlog"

testT1 :: [SpecialistNote] -> TestTree
testT1 notes = testCase "T1" $ do
  length notes == 2 @?
    "expect exactly two overloaded calls"
  any (includesDictLabel "$fShowX") notes @?
    "expect a call to include the Show X dictionary"
  case find (includesDictLabel "$fShowList") notes of
    Just SpecialistNote{..} -> do
      case specialistNoteDictInfos of
        [showListDict] ->
          case dictInfoFreeDicts showListDict of
            [showXDict] ->
              case dictInfoProv showXDict of
                Just InfoProv{..} ->
                  "$fShowX" == ipLabel @?
                    "expect the Show X dictionary to be the superclass referenced by the ShowList constructor"
                Nothing ->
                  assertFailure "expect to be able to find the info table provenance of the superclass dictionary"
            _ ->
              assertFailure "expect exactly one superclass referenced in the call including the Show List dictionary"
        _ ->
          assertFailure "expect one dictionary in the call that includes the ShowList dictionary constructor"
    Nothing ->
      assertFailure "expect a call to include the ShowList dictionary constructor"

-- | Checks whether at least one of the dictionary infos in the note references
-- a dictionary with the given label string.
includesDictLabel :: String -> SpecialistNote -> Bool
includesDictLabel s SpecialistNote{..} =
    any (maybe False ((==s) . ipLabel) . dictInfoProv) $
      specialistNoteDictInfos
