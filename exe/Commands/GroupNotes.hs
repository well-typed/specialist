module Commands.GroupNotes where

import Commands.Common
import Utils

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin.Types

import Control.Monad
import Data.List
import Options.Applicative
import Text.Read (readMaybe)

data GroupNotesOptions =
    GroupNotesOptions
      { groupNotesOptionsGetNotes :: IO (Either String [SpecialistNote])
      , groupNotesOptionsGroupOn :: GroupNotesOn
      , groupNotesOptionsSep :: Maybe String
      }

data GroupNotesOn =
      GroupNotesOnId
    | GroupNotesOnDictInfos
    | GroupNotesOnFunctionIpe
    | GroupNotesOnLocationLabel
  deriving Read

-- | Parse the options for the @group@ command.
groupNotesOptions :: Parser GroupNotesOptions
groupNotesOptions =
        GroupNotesOptions
    <$> notesInput
    <*> groupNotesOn
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

interpretGroupNotesCommand :: GroupNotesOptions -> IO ()
interpretGroupNotesCommand GroupNotesOptions{..} =
    groupNotesOptionsGetNotes >>=
      \case
        Right notes@(n:_) ->
          foldM_ go n $ sortBy comparison notes
        Right [] -> return ()
        Left msg ->
          perish $ "failed to get specialist notes from the input: " ++ msg
  where
    go :: SpecialistNote -> SpecialistNote -> IO SpecialistNote
    go prev n = do
      case (comparison prev n, groupNotesOptionsSep) of
        (EQ, _) ->
          return ()
        (_, Just sep) ->
          putStrLn sep
        _ -> return ()
      print n >> return n

    comparison :: SpecialistNote -> SpecialistNote -> Ordering
    comparison =
      case groupNotesOptionsGroupOn of
        GroupNotesOnId -> compareId
        GroupNotesOnDictInfos -> compareDictInfos
        GroupNotesOnFunctionIpe -> compareFunctionIpe
        GroupNotesOnLocationLabel -> compareLocationLabel
