module Commands where

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

data SpecialyzeCommand =
    ListNotesCommand ListNotesOptions
  | GroupNotesCommand GroupNotesOptions
  | DictProvenanceCommand DictProvenanceOptions
  | FindDuplicateSpecsCommand FindDuplicateSpecsOptions
  | PragmasCommand PragmasOptions
  | ToSpeedscopeCommand ToSpeedscopeOptions

data InputFormat = EventLogFormat | TextFormat

specialyzeCommand :: Parser SpecialyzeCommand
specialyzeCommand =
    subparser
      (    command "list-notes" listNotesInfo
        <> command "group-notes" groupNotesInfo
        <> command "dict-provenance" dictProvenanceInfo
        <> command
             "find-duplicate-specialisations"
             findDuplicateSpecialisationsInfo
        <> command
             "pragmas"
             pragmasInfo
        <> command
             "to-speedscope"
             toSpeedscopeInfo
      )
  where
    listNotesInfo :: ParserInfo SpecialyzeCommand
    listNotesInfo =
      info
        (ListNotesCommand <$> listNotesOptions)
        (progDesc "Print the input specialist notes in the order they occur")

    groupNotesInfo :: ParserInfo SpecialyzeCommand
    groupNotesInfo =
      info
        (GroupNotesCommand <$> groupNotesOptions)
        (progDesc "Print the input specialist notes grouped by some trait")

    dictProvenanceInfo :: ParserInfo SpecialyzeCommand
    dictProvenanceInfo =
      info
        (DictProvenanceCommand <$> dictProvenanceOptions)
        ( progDesc $
            "Print a mapping from dictionaries to provenance nodes in the " ++
            "call graph, as determined from the input specialist notes"
        )

    findDuplicateSpecialisationsInfo :: ParserInfo SpecialyzeCommand
    findDuplicateSpecialisationsInfo =
      info
        (FindDuplicateSpecsCommand <$> findDuplicateSpecsOptions)
        ( progDesc $
            "Find specialisations of the same overloaded function at the " ++
            "same type originating from different modules"
        )

    toSpeedscopeInfo :: ParserInfo SpecialyzeCommand
    toSpeedscopeInfo =
      info
        (ToSpeedscopeCommand <$> toSpeedscopeOptions)
        ( progDesc $
            "Make a speedscope formatted JSON file whose samples are " ++
            "constructed using the overloaded calls in the specialist output"
        )

    pragmasInfo :: ParserInfo SpecialyzeCommand
    pragmasInfo =
      info
        (PragmasCommand <$> pragmasOptions)
        ( progDesc $
            "Get a summary of the most executed overloaded calls that could " ++
            "potentially be specialised using pragmas"
        )

-------------------------------------------------------------------------------
-- Interpreting commands
-------------------------------------------------------------------------------

interpretSpecialyzeCommand :: SpecialyzeCommand -> IO ()
interpretSpecialyzeCommand =
    \case
      ListNotesCommand opts ->
        interpretListNotesCommand opts
      GroupNotesCommand opts ->
        interpretGroupNotesCommand opts
      DictProvenanceCommand opts ->
        interpretDictProvenanceCommand opts
      FindDuplicateSpecsCommand opts ->
        interpretFindDuplicateSpecsCommand opts
      PragmasCommand opts ->
        interpretPragmasCommand opts
      ToSpeedscopeCommand opts ->
        interpretToSpeedscopeCommand opts
