-- |
-- Functions for extracting and analyzing specialist plugin output from a GHC
-- event log.

module GHC.Specialist.Analysis.EventLog where

import GHC.Specialist.Plugin.Types

import Data.Text (unpack)
import GHC.RTS.Events
import Text.Read

-- | Extract a 'SpecialistNote' from an 'Event'
specialistNoteFromEvent :: Event -> Maybe SpecialistNote
specialistNoteFromEvent Event{..} =
    case evSpec of
      UserMessage msg ->
        readMaybe @SpecialistNote $ unpack msg
      _ -> Nothing

specialistNotesFromEventLogFile :: FilePath -> IO [SpecialistNote]
specialistNotesFromEventLogFile eventLogFile =
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          return $ go (eventLogEvents eventLog)
        Left msg ->
          fail msg
  where
    go :: [Event] -> [SpecialistNote]
    go [] = []
    go (ev:evs) =
        case specialistNoteFromEvent ev of
          Just note -> note : go evs
          Nothing -> go evs

eventLogEvents :: EventLog -> [Event]
eventLogEvents (EventLog _ (Data evs)) = evs
