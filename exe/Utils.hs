module Utils where

import System.Exit

perishUsage :: String -> IO ()
perishUsage msg = do
  putStrLn ("specialyze error: " ++ msg)
  putStrLn "  usage: specialyze <eventlog>"
  exitFailure

perish :: String -> IO ()
perish msg = do
  putStrLn ("specialyze error: " ++ msg)
  exitFailure
