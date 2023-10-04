-- |
-- Functions for extracting and analyzing specialist plugin output from a GHC
-- event log.

module GHC.Specialist.Analysis.EventLog where

import GHC.Specialist.Plugin.Types

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Text (unpack)
import GHC.RTS.Events
import Text.Read

 -------------------------------------------------------------------------------
 -- * Folds
 -------------------------------------------------------------------------------

-- Right folds

foldrEventLogFileSpecialistNotes
  :: (SpecialistNote -> b -> b)
  -> b
  -> FilePath
  -> IO (Either String b)
foldrEventLogFileSpecialistNotes f acc eventLogFile = do
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          return . Right $ foldrEventLogSpecialistNotes f acc eventLog
        Left msg ->
          return . Left $ msg

foldrEventLogSpecialistNotes
  :: (SpecialistNote -> b -> b)
  -> b
  -> EventLog
  -> b
foldrEventLogSpecialistNotes f acc =
    foldrEventsSpecialistNotes f acc . eventLogEvents

foldrEventsSpecialistNotes
  :: (SpecialistNote -> b -> b)
  -> b
  -> [Event]
  -> b
foldrEventsSpecialistNotes = foldEventsSpecialistNotes (foldr . flip) . flip

-- Left folds

foldlEventLogFileSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> FilePath
  -> IO (Either String b)
foldlEventLogFileSpecialistNotes f acc eventLogFile = do
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          return . Right $ foldlEventLogSpecialistNotes f acc eventLog
        Left msg ->
          return . Left $ msg

foldlEventLogSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> EventLog
  -> b
foldlEventLogSpecialistNotes f acc =
    foldlEventsSpecialistNotes f acc . eventLogEvents

foldlEventsSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> [Event]
  -> b
foldlEventsSpecialistNotes = foldEventsSpecialistNotes foldl

-- Strict left folds

foldl'EventLogFileSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> FilePath
  -> IO (Either String b)
foldl'EventLogFileSpecialistNotes f acc eventLogFile = do
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          return . Right $ foldl'EventLogSpecialistNotes f acc eventLog
        Left msg ->
          return . Left $ msg

foldl'EventLogSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> EventLog
  -> b
foldl'EventLogSpecialistNotes f acc =
    foldl'EventsSpecialistNotes f acc . eventLogEvents

foldl'EventsSpecialistNotes
  :: (b -> SpecialistNote -> b)
  -> b
  -> [Event]
  -> b
foldl'EventsSpecialistNotes = foldEventsSpecialistNotes foldl'

-- Monadic folds

foldMEventLogFileSpecialistNotes
  :: MonadIO m
  => (b -> SpecialistNote -> m b)
  -> b
  -> FilePath
  -> m (Either String b)
foldMEventLogFileSpecialistNotes f acc eventLogFile =
    liftIO (readEventLogFromFile eventLogFile) >>=
      \case
        Right eventLog ->
          Right <$> foldMEventLogSpecialistNotes f acc eventLog
        Left msg ->
          return . Left $ msg

foldMEventLogSpecialistNotes
  :: Monad m
  => (b -> SpecialistNote -> m b)
  -> b
  -> EventLog
  -> m b
foldMEventLogSpecialistNotes f acc =
    foldMEventsSpecialistNotes f acc . eventLogEvents

foldMEventsSpecialistNotes
  :: Monad m
  => (b -> SpecialistNote -> m b)
  -> b
  -> [Event]
  -> m b
foldMEventsSpecialistNotes f =
    foldM go
  where
    go acc ev =
      case specialistNoteFromEvent ev of
        Just note -> f acc note
        Nothing -> return acc

foldEventsSpecialistNotes
  :: ((b -> Event -> b) -> b -> [Event] -> b)
  -> (b -> SpecialistNote -> b)
  -> b
  -> [Event]
  -> b
foldEventsSpecialistNotes fold f =
    fold go
  where
    go acc ev =
      case specialistNoteFromEvent ev of
        Just note -> f acc note
        Nothing -> acc

-------------------------------------------------------------------------------
-- * Utilities
-------------------------------------------------------------------------------

-- | Extract a 'SpecialistNote' from an 'Event'
specialistNoteFromEvent :: Event -> Maybe SpecialistNote
specialistNoteFromEvent Event{..} =
    case evSpec of
      UserMessage msg ->
        readMaybe @SpecialistNote $ unpack msg
      _ -> Nothing

specialistNotesFromEventLogFile :: FilePath -> IO (Either String [SpecialistNote])
specialistNotesFromEventLogFile = foldrEventLogFileSpecialistNotes (:) []

specialistNotesFromEventLog :: EventLog -> [SpecialistNote]
specialistNotesFromEventLog = foldrEventLogSpecialistNotes (:) []

eventLogEvents :: EventLog -> [Event]
eventLogEvents (EventLog _ (Data evs)) = evs
