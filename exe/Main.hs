module Main where

import GHC.Specialist.Analyze

import System.Environment
import System.Exit

main :: IO ()
main =
    getArgs >>=
      \case
        (eventLogFile:_) ->
          analysis eventLogFile
        [] ->
          perishUsage "expected an argument"

perishUsage :: String -> IO ()
perishUsage msg = do
  putStrLn ("specialyze error: " ++ msg)
  putStrLn "  usage: specialyze <eventlog>"
  exitFailure

perish :: String -> IO ()
perish msg = do
  putStrLn ("specialyze error: " ++ msg)
  exitFailure

analysis :: FilePath -> IO ()
analysis eventLogFile = do
    print =<< specialistNotesFromEventLogFile eventLogFile
