module Commands where

import Utils

import GHC.Specialist.Analysis
import GHC.Specialist.Types

import Control.Monad
import Data.List
import Options.Applicative
import Text.Read

-------------------------------------------------------------------------------
-- Parsing commands
-------------------------------------------------------------------------------

data SpecialyzeCommand =
    ListNotesCommand
  | GroupNotesCommand GroupNotesOptions

specialyzeCommand :: Parser (FilePath, SpecialyzeCommand)
specialyzeCommand =
        (,)
    <$> eventLogFilePath
    <*> subparser
          (    command "list-notes" listNotesInfo
            <> command "group-notes" groupNotesInfo
          )
  where
    eventLogFilePath :: Parser FilePath
    eventLogFilePath =
      strOption
        (    long "eventlog"
          <> short 'f'
          <> metavar "EVENTLOG"
          <> help "Specify the path to the eventlog"
        )

    listNotesInfo :: ParserInfo SpecialyzeCommand
    listNotesInfo =
      info
        (pure ListNotesCommand)
        (progDesc "Print the list of specialist notes in the order they occur in the eventlog")

    groupNotesInfo :: ParserInfo SpecialyzeCommand
    groupNotesInfo =
      info
        (GroupNotesCommand <$> groupNotesOptions)
        (progDesc "Print the specialist notes, grouped by some common trait")

-------------------------------------------------------------------------------
-- Interpreting commands
-------------------------------------------------------------------------------

interpretSpecialyzeCommand :: FilePath -> SpecialyzeCommand -> IO ()
interpretSpecialyzeCommand eventLogFile =
    \case
      ListNotesCommand ->
        interpretListNotesCommand eventLogFile
      GroupNotesCommand GroupNotesOptions{..} ->
        interpretGroupNotesCommand eventLogFile
          groupNotesOptionsGroupNotesOn
          groupNotesOptionsSep

-------------------------------------------------------------------------------
-- list-notes command
-------------------------------------------------------------------------------

interpretListNotesCommand :: FilePath -> IO ()
interpretListNotesCommand eventLogFile =
    specialistNotesFromEventLogFile eventLogFile >>=
      \case
        Right notes -> mapM_ print notes
        Left _ -> perish "failed to get specialist notes from the eventlog"

-------------------------------------------------------------------------------
-- group-notes command
-------------------------------------------------------------------------------

data GroupNotesOptions =
    GroupNotesOptions
      { groupNotesOptionsGroupNotesOn :: GroupNotesOn
      , groupNotesOptionsSep :: Maybe String
      }

data GroupNotesOn =
      GroupNotesOnId
    | GroupNotesOnDictInfos
    | GroupNotesOnFunctionIpe
    | GroupNotesOnLocationLabel
  deriving Read

-- | Parse the options for the @recent-observations@ command.
groupNotesOptions :: Parser GroupNotesOptions
groupNotesOptions =
        GroupNotesOptions
    <$> groupNotesOn
    <*> optional
          ( strOption
              (    long "separator"
                <> short 's'
                <> metavar "STRING"
                <> help "Separate the groups of notes with a line containing the provided string in the output"
              )
          )
    <**> helper

groupNotesOn :: Parser GroupNotesOn
groupNotesOn =
    option (maybeReader parseGroupNotesOn)
      (    long "group-on"
        <> help
             ( "Specify the trait to group the specialist notes on " ++
               "(\"id\", \"dict-info\", \"function-ipe\", or \"location-label\")"
             )
      )

parseGroupNotesOn :: String -> Maybe GroupNotesOn
parseGroupNotesOn "id"             = Just GroupNotesOnId
parseGroupNotesOn "dict-info"      = Just GroupNotesOnDictInfos
parseGroupNotesOn "function-ipe"   = Just GroupNotesOnFunctionIpe
parseGroupNotesOn "location-label" = Just GroupNotesOnLocationLabel
parseGroupNotesOn x                = readMaybe x

interpretGroupNotesCommand :: FilePath -> GroupNotesOn -> Maybe String -> IO ()
interpretGroupNotesCommand eventLogFile groupOn msep =
    specialistNotesFromEventLogFile eventLogFile >>=
      \case
        Right notes -> foldM_ go () $ grouper notes
        Left _ -> perish "failed to get specialist notes from the eventlog"
  where
    go :: () -> [SpecialistNote] -> IO ()
    go () notes = do
      mapM_ print notes
      case msep of
        Just sep -> putStrLn sep
        Nothing -> return ()

    grouper :: [SpecialistNote] -> [[SpecialistNote]]
    grouper =
      case groupOn of
        GroupNotesOnId -> groupBy eqById
        GroupNotesOnDictInfos -> groupBy eqByDictInfos
        GroupNotesOnFunctionIpe -> groupBy eqByFunctionIpe
        GroupNotesOnLocationLabel -> groupBy eqByLocationLabel
