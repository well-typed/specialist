module Commands.Common where

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin.Types

import Options.Applicative

notesInput :: Parser (IO (Either String [SpecialistNote]))
notesInput =
    eventLogFile <|> textFile
  where
    eventLogFile :: Parser (IO (Either String [SpecialistNote]))
    eventLogFile = specialistNotesFromEventLogFile <$> eventLogFileInput

    textFile :: Parser (IO (Either String [SpecialistNote]))
    textFile = specialistNotesFromFile <$>
      strOption
        (    long "text"
          <> metavar "FILE"
          <> help
               ( "Specify the location of a text file containing the " ++
                 "readable specialist notes, separated by newlines"
               )
        )

eventLogFileInput :: Parser FilePath
eventLogFileInput =
    strOption
      (    long "eventlog"
        <> metavar "EVENTLOG"
        <> help "Specify the location of an eventlog containing the specialist output"
      )