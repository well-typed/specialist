module Main where

import Commands

import Options.Applicative

main :: IO ()
main = do
    execParser opts >>= uncurry interpretSpecialyzeCommand
  where
    opts :: ParserInfo (FilePath, SpecialyzeCommand)
    opts =
      info
        (specialyzeCommand <**> helper)
        (    header "specialyze"
          <> progDesc "Analyze output from the specialist plugin"
        )
