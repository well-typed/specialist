module Commands.ListNotes where

import Commands.Common
import Utils

import GHC.Specialist.Plugin.Types

import Options.Applicative

data ListNotesOptions =
    ListNotesOptions
      { listNotesOptionsGetNotes :: IO (Either String [SpecialistNote])
      }

-- | Parse the options for the @list@ command.
listNotesOptions :: Parser ListNotesOptions
listNotesOptions =
         ListNotesOptions <$> notesInput
    <**> helper

interpretListNotesCommand :: ListNotesOptions -> IO ()
interpretListNotesCommand ListNotesOptions{..} =
    listNotesOptionsGetNotes >>=
      \case
        Right notes ->
          mapM_ print notes
        Left msg ->
          perish $ "failed to get specialist notes from the input: " <> msg