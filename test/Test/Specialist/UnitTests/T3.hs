module Test.Specialist.UnitTests.T3 where

import GHC.Specialist
import Test.Specialist.Types
import Test.Specialist.Utils

import Control.Monad
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
    mapM_ checkSuperclasses notes
  where
    checkDictCount :: (SpecialistNote, Int) -> Assertion
    checkDictCount (n, c) =
      length (specialistNoteDictInfos n) == c @?
        "expect " ++ show c ++ " dictionaries in the overloaded call"

    checkSuperclasses :: SpecialistNote -> Assertion
    checkSuperclasses SpecialistNote{..} =
      mapM_ checkDictSuperclasses specialistNoteDictInfos

    checkDictSuperclasses :: DictInfo -> Assertion
    checkDictSuperclasses di@DictInfo{..} = do
      when (dictIncludesDictName "C:Ord" di) $
        any (dictIncludesDictName "C:Eq") dictInfoFreeDicts @?
          "expect Eq to be a superclass of Ord"
      when (dictIncludesDictName "C:Real" di) $ do
        any (dictIncludesDictName "C:Num") dictInfoFreeDicts @?
          "expect Num to be a superclass of Real"
        any (dictIncludesDictName "C:Ord") dictInfoFreeDicts @?
          "expect Ord to be a superclass of Real"
      when (dictIncludesDictName "C:Integral" di) $ do
        any (dictIncludesDictName "C:Real") dictInfoFreeDicts @?
          "expect Real to be a superclass of Integral"
        any (dictIncludesDictName "C:Enum") dictInfoFreeDicts @?
          "expect Enum to be a superclass of Integral"
      mapM_ checkDictSuperclasses dictInfoFreeDicts
