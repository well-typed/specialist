module Test.Specialist.UnitTests.T1 where

import GHC.Specialist
import Test.Specialist.Types

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
      Left  _ ->
        error "failed to read specialist notes from test-T1.eventlog"
      Right notes -> do
        removeFile "test-T1.eventlog"
        return notes

testT1 :: [SpecialistNote] -> TestTree
testT1 notes = testCase "T1" $ do
  length notes == 2 @?
    "Expect exactly two overloaded calls"
  any (includesDictLabel "$fShowX") notes @?
    "Expect a call to include the Show X dictionary"
  any (includesDictLabel "$fShowList") notes @?
    "Expect a call to include the show list dictionary constructor for X"

-- | Checks whether at least one of the dictionary infos in the note references
-- a dictionary with the given label string.
includesDictLabel :: String -> SpecialistNote -> Bool
includesDictLabel s SpecialistNote{..} =
    any (maybe False ((==s) . ipLabel) . dictInfoProv) $
      specialistNoteDictInfos
