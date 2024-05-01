module Test.Specialist.UnitTests.T1 where

import GHC.Specialist
import Test.Specialist.Types
import Test.Specialist.Utils

import System.Directory
import System.Process
-- import GHC.InfoProv
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
    notes <- specialistNotesFromEventLogFile "test-T1.eventlog"
    removeFile "test-T1.eventlog"
    return notes

testT1 :: [SpecialistNote] -> TestTree
testT1 notes = testCase "T1" $ do
    length notes == 2 @?
      "expect exactly two overloaded calls"
    notes' <-
      case findAndRemove (includesDictLabel "$fShowX") notes of
        Just (_, notes'@[_]) ->
          return notes'
        _ ->
          assertFailure "expect one call to include the $fShowX dictionary"
    showListNote <-
      case findAndRemove (includesDictLabel "$fShowList") notes' of
        Just (note, []) ->
          return note
        _ ->
          assertFailure "expect one call to include the $fShowList dictionary constructor"
    _showListDict <-
      case specialistNoteDictInfos showListNote of
        [d] ->
          return $ dictInfoClosure d
        _ ->
          assertFailure "expect one dictionary in the call that includes the ShowList dictionary constructor"
    return ()
    -- TODO: This fails because we currently falsely identify regular functions
    -- as dictionaries.
    -- showXDict <-
    --   case dictClosureFrees showListDict of
    --     [d] ->
    --       return d
    --     _ ->
    --       assertFailure "expect exactly one superclass referenced in the call including the $fShowList dictionary"
    -- case dictClosureIpe showXDict of
    --   Just InfoProv{..} ->
    --     "$fShowX" == ipLabel @?
    --       "expect the $fShowX dictionary to be the superclass referenced by the $fShowList constructor"
    --   Nothing ->
    --     assertFailure "expect to be able to find the info table provenance of the superclass dictionary"
