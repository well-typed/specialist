module Commands where

import GHC.Specialist.Plugin.Types
import GHC.Specialist.Analysis.EventLog
import GHC.Specialist.Analysis.Rank
import GHC.Specialist.Analysis.SpecChains
import GHC.Specialist.Analysis.Speedscope

import Commands.DictProvenance
import Commands.FindDuplicateSpecs
import Commands.GroupNotes

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
      ListNotesCommand
        -- | 'True': Print raw 'show'n notes. 'False': Pretty print
        !Bool

      -- | Group notes from the event log on some trait
    | GroupNotesCommand !GroupNotesOptions

      -- | Find the location of unknown calls that brought some opaque
      -- dictionaries into scope
    | DictProvenanceCommand

      -- | Get the calls that were evaluated most at specific dictionaries (most
      -- likely candidates for SPECIALIZE pragmas)
    | RankCommand
        -- | Omit modules
        [String]
        -- | Include modules
        [String]

      -- | Get the chains of consecutive overloaded nodes in the call graph for
      -- a given dictionary
    | SpecChainsCommand
        -- | Dictionary closure substring
        (Maybe String)
        -- | Only include chains with a cost center having this substring
        (Maybe String)

      -- | Convert the notes into speedscope JSON format where each sample
      -- corresponds to an overloaded call.
    | ToSpeedscopeCommand
        -- | Write the output JSON to this file (defaults to
        -- <event-log-file-name>.json)
        (Maybe FilePath)

        -- | Only include samples referencing the dictionary at this source
        -- location (even as a superclass)
        (Maybe String)
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
      (    command "list"            infoListNotes
        <> command "rank"            infoRank
        <> command "spec-chains"     infoSpecChains
        <> command "group-notes"     infoGroupNotes
        <> command "dict-provenance" infoDictProvenance
        <> command "speedscope"      infoSpeedscope
      )
  where
    infoListNotes :: ParserInfo NotesCommand
    infoListNotes =
      info
        ( ListNotesCommand
            <$>
            switch
              (    long "raw"
                <> help "Do not pretty print the notes"
              )
            <**>
            helper
        )
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

    infoRank :: ParserInfo NotesCommand
    infoRank =
      info
        ( RankCommand
            <$>
            many
              ( strOption
                  (    long "omit-modules"
                    <> metavar "SUBSTRING"
                    <> help
                          ( "Omit overloaded functions originating from " ++
                            "modules whose names contain SUBSTRING"
                          )
                  )
              )
            <*>
            many
              ( strOption
                  (    long "include-modules"
                    <> metavar "SUBSTRING"
                    <> help
                          ( "Only include overloaded functions " ++
                            "originating from modules whose names contain " ++
                            "SUBSTRING"
                          )
                  )
              )
            <**>
            helper
        )
        ( progDesc $
            "Get a ranking of the most executed overloaded calls and what " ++
            "dictionaries they were called with."
        )

    infoSpecChains :: ParserInfo NotesCommand
    infoSpecChains =
      info
        ( SpecChainsCommand
            <$>
            optional
              ( strOption
                  (    long "dict"
                    <> metavar "SUBSTRING"
                    <> help
                          ( "Only find call chains involving dictionaries whose " ++
                            "info table provenance strings contain SUBSTRING"
                          )
                  )
              )
            <*>
            optional
              ( strOption
                  (    long "containing"
                    <> metavar "SUBSTRING"
                    <> help
                          ( "Only provide call chains including at least " ++
                            "one cost center label containing SUBSTRING"
                          )
                  )
              )
            <**>
            helper
        )
        ( progDesc $
            "Find chains of consecutive overloaded calls involving specific " ++
            "dictionaries"
        )

    infoSpeedscope :: ParserInfo NotesCommand
    infoSpeedscope =
      info
        ( ToSpeedscopeCommand
            <$>
            optional
              ( strOption
                  ( long "out"
                    <>
                    short 'o'
                    <>
                    metavar "FILE"
                    <>
                    help "Write the output JSON to this file path"
                  )
              )
            <*>
            optional
              ( strOption
                  ( long "dict-location"
                    <>
                    metavar "SUBSTRING"
                    <>
                    help
                      ( "Only consider samples referencing dictionaries " ++
                        "whose info table provenance string includes SUBSTRING"
                      )
                  )
              )
            <**> helper
        )
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
          ToSpeedscopeCommand mOutFile mDictLoc ->
            speedscope mOutFile mDictLoc inp
          cmd' -> do
            notes <- specialistNotesFromEventLogFile inp
            case cmd' of
              ListNotesCommand raw ->
                mapM_
                  ( if raw then
                      print
                    else
                      putStrLn . prettySpecialistNote
                  )
                  notes
              GroupNotesCommand opts ->
                groupNotes opts notes
              DictProvenanceCommand ->
                dictProvenance notes
              RankCommand omitModules includeModules ->
                putStrLn $
                  prettyRankMap $
                    rank omitModules includeModules notes
              SpecChainsCommand mDictFilt mCCFilt ->
                putStrLn $
                  prettySpecChainMap $
                    specChains mDictFilt mCCFilt notes
      FindDuplicateSpecsCommand opts ->
        findDuplicateSpecializations opts
