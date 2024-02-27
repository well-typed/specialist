module Commands.ListNotes where

import GHC.Specialist.Plugin.Types
import Options.Applicative


data ListNotesOptions =
      ListNotesOptions
        -- | 'True': Raw 'show' output. 'False': Pretty output
        !Bool
  deriving (Show, Read, Eq)

listNotesOptions :: Parser ListNotesOptions
listNotesOptions =
    ListNotesOptions <$>
      switch
        (    long "raw"
          <> help "Do not pretty print the output"
        )

listNotes :: ListNotesOptions -> [SpecialistNote] -> IO ()
listNotes (ListNotesOptions raw) =
    mapM_ $
      if raw then
        print
      else
        putStrLn . prettyPrint
