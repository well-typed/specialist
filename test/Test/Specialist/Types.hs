module Test.Specialist.Types where

import GHC.Specialist

import Test.Tasty

data TestSpec =
    ExeTestSpec
      { -- | Get the plugin output from an executable
        exeTestSpecGetNotes :: IO [SpecialistNote]

        -- | Assertion on the specialist notes that come out of the test
        -- executable. First argument is a step function for tracing test case
        -- progress, e.g. from 'testCaseSteps'.
      , exeTestSpecTest :: [SpecialistNote] -> TestTree
      }
