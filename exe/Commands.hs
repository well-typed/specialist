module Commands where

import Utils

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin.Types

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

data InputFormat = EventLogFormat | TextFormat

specialyzeCommand
  :: Parser (IO (Either String [SpecialistNote]), SpecialyzeCommand)
specialyzeCommand =
        (,)
    <$> (eventLogFile <|> textFile)
    <*> subparser
          (    command "list" listNotesInfo
            <> command "group" groupNotesInfo
          )
  where
    eventLogFile :: Parser (IO (Either String [SpecialistNote]))
    eventLogFile = specialistNotesFromEventLogFile <$>
      strOption
        (    long "eventlog"
          <> metavar "EVENTLOG"
          <> help "Specify the location of an eventlog containing the specialist output"
        )

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

interpretSpecialyzeCommand
  :: IO (Either String [SpecialistNote])
  -> SpecialyzeCommand
  -> IO ()
interpretSpecialyzeCommand getNotes =
    \case
      ListNotesCommand ->
        interpretListNotesCommand getNotes
      GroupNotesCommand GroupNotesOptions{..} ->
        interpretGroupNotesCommand getNotes
          groupNotesOptionsGroupNotesOn
          groupNotesOptionsSep

-------------------------------------------------------------------------------
-- list-notes command
-------------------------------------------------------------------------------

interpretListNotesCommand :: IO (Either String [SpecialistNote]) -> IO ()
interpretListNotesCommand getNotes =
    getNotes >>=
      \case
        Right notes ->
          mapM_ print notes
        Left msg ->
          perish $ "failed to get specialist notes from the input: " <> msg

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
      (    long "on"
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

interpretGroupNotesCommand :: IO (Either String [SpecialistNote]) -> GroupNotesOn -> Maybe String -> IO ()
interpretGroupNotesCommand getNotes groupOn msep =
    getNotes >>=
      \case
        Right notes@(n:_) ->
          foldM_ go n $ sortBy comparison notes
        Right [] -> return ()
        Left msg ->
          perish $ "failed to get specialist notes from the input: " ++ msg
  where
    go :: SpecialistNote -> SpecialistNote -> IO SpecialistNote
    go prev n = do
      case (comparison prev n, msep) of
        (EQ, _) ->
          return ()
        (_, Just sep) ->
          putStrLn sep
        _ -> return ()
      print n >> return n

    comparison :: SpecialistNote -> SpecialistNote -> Ordering
    comparison =
      case groupOn of
        GroupNotesOnId -> compareId
        GroupNotesOnDictInfos -> compareDictInfos
        GroupNotesOnFunctionIpe -> compareFunctionIpe
        GroupNotesOnLocationLabel -> compareLocationLabel
