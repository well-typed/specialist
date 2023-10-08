module Commands where

import Utils

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin.Types

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Options.Applicative
import Text.Read

-------------------------------------------------------------------------------
-- Parsing commands
-------------------------------------------------------------------------------

data SpecialyzeCommand =
    ListNotesCommand
  | GroupNotesCommand GroupNotesOptions
  | DictProvenanceCommand

data InputFormat = EventLogFormat | TextFormat

specialyzeCommand
  :: Parser (IO (Either String [SpecialistNote]), SpecialyzeCommand)
specialyzeCommand =
        (,)
    <$> (eventLogFile <|> textFile)
    <*> subparser
          (    command "list" listNotesInfo
            <> command "group" groupNotesInfo
            <> command "dict-provenance" dictProvenanceInfo
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

    dictProvenanceInfo :: ParserInfo SpecialyzeCommand
    dictProvenanceInfo =
      info
        (pure DictProvenanceCommand)
        (progDesc "Print a mapping from dictionaries to provenance nodes in the call graph")

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
      DictProvenanceCommand ->
        interpretDictProvenanceCommand getNotes

-------------------------------------------------------------------------------
-- list command
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
-- group command
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
              (    long "sep"
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

-------------------------------------------------------------------------------
-- dict-provenance
-------------------------------------------------------------------------------

interpretDictProvenanceCommand :: IO (Either String [SpecialistNote]) -> IO ()
interpretDictProvenanceCommand getNotes =
    getNotes >>=
      \case
        Right notes ->
          prettyPrint $ foldl' go Map.empty notes
        Left msg ->
          perish $ "failed to get specialist notes from the input: " <> msg
  where
    go
      :: Map DictInfo (Set [String])
      -> SpecialistNote
      -> Map DictInfo (Set [String])
    go acc SpecialistNote{..} =
      foldl' updateProvs acc (map (,specialistNoteCcs) $ catMaybes specialistNoteDictInfos)

    updateProvs
      :: Map DictInfo (Set [String])
      -> (DictInfo, [String])
      -> Map DictInfo (Set [String])
    updateProvs acc (_,[]) =
      -- We have no call-stack data here, return
      acc
    updateProvs acc (d,path) =
      -- We have a non-empty call-stack
      case acc Map.!? d of
        Nothing ->
          -- This dictionary has not been seen, insert this path and return
          Map.insert d (Set.singleton path) acc
        Just paths ->
          -- We have recorded paths for this dictionary
          let
            -- Paths which the current path is prefixed by
            prefixedBy = Set.filter (`isPrefixOf` path) paths

            -- Paths which the current path is a prefix of, and paths which it
            -- is not a prefix of
            (prefixOf, notPrefixOf) = Set.partition (path `isPrefixOf`) paths
          in
            if not $ Set.null prefixedBy then
              -- We have already tracked a prefix of the current path, return
              acc
            else if not $ Set.null prefixOf then
              -- The current path is a prefix of some of these paths, add this
              -- path and remove the ones which it is a prefix of
              Map.insert d (Set.insert path notPrefixOf) acc
            else
              -- The current path is not a prefix of or prefixed by any current
              -- paths, just add it
              Map.insert d (Set.insert path paths) acc

    prettyPrint
      :: Map DictInfo (Set [String])
      -> IO ()
    prettyPrint result = do
      putStrLn "Dictionary provenances:"
      void $ Map.traverseWithKey prettyPrintDictProv result

    prettyPrintDictProv :: DictInfo -> Set [String] -> IO ()
    prettyPrintDictProv d paths = do
      putStrLn "  dictionary:"
      putStrLn $ "    " ++ show d
      putStrLn "  provenances:"
      mapM_
          prettyPrintCallStack
        . sortBy (\p1 p2 -> compare (length p1) (length p2))
        $ Set.elems paths
      putStrLn ""

    prettyPrintCallStack :: [String] -> IO ()
    prettyPrintCallStack ccs =
      putStrLn $ unlines $
          "    CC stack:"
        : map ("      " ++) ccs
