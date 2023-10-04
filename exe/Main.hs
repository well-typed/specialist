module Main where

import GHC.Specialist.Plugin.Types

import Commands

import Options.Applicative

main :: IO ()
main = do
    execParser opts >>= uncurry interpretSpecialyzeCommand
  where
    opts :: ParserInfo (IO (Either String [SpecialistNote]), SpecialyzeCommand)
    opts =
      info
        (specialyzeCommand <**> helper)
        (    header "specialyze"
          <> progDesc "Analyze output from the specialist plugin"
        )
