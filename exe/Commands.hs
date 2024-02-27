module Commands where

import GHC.Specialist.Analysis.EventLog

import Commands.DictProvenance
import Commands.FindDuplicateSpecs
import Commands.GroupNotes
import Commands.ListNotes
import Commands.Pragmas
import Commands.ToSpeedscope

import Options.Applicative

-------------------------------------------------------------------------------
-- Parsing commands
-------------------------------------------------------------------------------

-- | @specialyze@ has two top-level notions of a command:
--
-- 1. Notes commands: Commands dealing with notes output by the plugin
-- 2. Auxiliary commands: Commands aiming to help with auxiliary specialization
--    analysis, e.g. finding duplicate specializations.
data SpecialyzeCommand =
      NotesCommand !FilePath !NotesCommand

    | -- | Find duplicate specializations in some GHC dump output (requires
      -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11358)
      FindDuplicateSpecsCommand FindDuplicateSpecsOptions
  deriving (Show, Read, Eq)

data NotesCommand =
      -- | List notes from the event log in order, either prettified or raw
      ListNotesCommand !ListNotesOptions

      -- | Group notes from the event log on some trait
    | GroupNotesCommand !GroupNotesOptions

      -- | Find the location of unknown calls that brought some opaque
      -- dictionaries into scope
    | DictProvenanceCommand

      -- | Get the calls that were evaluated most at specific dictionaries (most
      -- likely candidates for SPECIALIZE pragmas)
    | PragmasCommand !PragmasOptions

      -- | Convert the notes into speedscope JSON format where each sample
      -- corresponds to an overloaded call.
    | ToSpeedscopeCommand !ToSpeedscopeOptions
  deriving (Show, Read, Eq)

-- | Top-level command parser
specialyzeCommand :: Parser SpecialyzeCommand
specialyzeCommand =
        subparser (command "notes" infoNotes)
    <|> auxCommands
  where
    infoNotes :: ParserInfo SpecialyzeCommand
    infoNotes =
        info
          (NotesCommand <$> notesInput <*> notesCommand <**> helper)
          (progDesc "Analyze specialist notes from an event log")

    notesInput :: Parser FilePath
    notesInput =
        strOption
          (    long "eventlog"
            <> short 'i'
            <> metavar "EVENT LOG"
            <> help
                  ( "Specify the location of the event log file containing the " ++
                    "specialist output"
                  )
          )

-- | @notes@ command parser
notesCommand :: Parser NotesCommand
notesCommand =
    subparser
      (    command "list-notes"      infoListNotes
        <> command "group-notes"     infoGroupNotes
        <> command "dict-provenance" infoDictProvenance
        <> command "pragmas"         infoPragmas
        <> command "to-speedscope"   infoToSpeedscope
      )
  where
    infoListNotes :: ParserInfo NotesCommand
    infoListNotes =
      info
        (ListNotesCommand <$> listNotesOptions)
        (progDesc "Print the notes in the order they occur in the input")

    infoGroupNotes :: ParserInfo NotesCommand
    infoGroupNotes =
      info
        (GroupNotesCommand <$> groupNotesOptions)
        (progDesc "Print the notes grouped by some trait")

    infoDictProvenance :: ParserInfo NotesCommand
    infoDictProvenance =
      info
        (pure DictProvenanceCommand)
        ( progDesc $
            "Print a mapping from dictionaries to provenance nodes in the " ++
            "call graph, as determined from the input notes"
        )

    infoPragmas :: ParserInfo NotesCommand
    infoPragmas =
      info
        (PragmasCommand <$> pragmasOptions)
        ( progDesc $
            "Get a ranking of the most executed overloaded calls and what " ++
            "dictionaries they were called with. Helpful for coming up " ++
            "with SPECIALIZE pragmas."
        )

    infoToSpeedscope :: ParserInfo NotesCommand
    infoToSpeedscope =
      info
        (ToSpeedscopeCommand <$> toSpeedscopeOptions)
        ( progDesc $
            "Make a speedscope formatted JSON file whose samples are " ++
            "constructed using the overloaded calls in the specialist output"
        )

-- | Auxiliary command parser
auxCommands :: Parser SpecialyzeCommand
auxCommands =
    subparser
      ( command "find-duplicate-specializations" infoFindDuplicateSpecializations
      )
  where
    infoFindDuplicateSpecializations :: ParserInfo SpecialyzeCommand
    infoFindDuplicateSpecializations =
      info
        (FindDuplicateSpecsCommand <$> findDuplicateSpecsOptions)
        ( progDesc $
            "Find specialisations of the same overloaded function at the " ++
            "same type originating from different modules"
        )

-------------------------------------------------------------------------------
-- Interpreting commands
-------------------------------------------------------------------------------

-- | Dispatch to the appropriate command logic
interpretSpecialyzeCommand :: SpecialyzeCommand -> IO ()
interpretSpecialyzeCommand =
    \case
      NotesCommand inp cmd -> do
        case cmd of
          ToSpeedscopeCommand opts ->
            toSpeedscope opts inp
          cmd' -> do
            notes <- specialistNotesFromEventLogFile inp
            case cmd' of
              ListNotesCommand opts ->
                listNotes opts notes
              GroupNotesCommand opts ->
                groupNotes opts notes
              DictProvenanceCommand ->
                dictProvenance notes
              PragmasCommand opts ->
                pragmas opts notes
      FindDuplicateSpecsCommand opts ->
        findDuplicateSpecializations opts
