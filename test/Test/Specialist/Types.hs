module Test.Specialist.Types where

import GHC.Specialist

import Test.Tasty

data TestSpec =
      ExeTestSpec
        { -- | Get the plugin output from an executable
          exeTestSpecGetNotes :: IO [SpecialistNote]

          -- | Assertion on the specialist notes that come out of the test
          -- executable.
        , exeTestSpecTest :: [SpecialistNote] -> TestTree
        }
    | ShouldCompileTestSpec
        { -- | Action to run that should succeed if the test successfully compiled
          shouldCompileTestSpecExe :: TestTree
        }
