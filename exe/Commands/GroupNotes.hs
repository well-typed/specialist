module Commands.GroupNotes where

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin.Types

import Control.Monad
import Data.List
import Options.Applicative
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Parsing options
-------------------------------------------------------------------------------

data GroupNotesOptions =
    GroupNotesOptions
      -- | How to group the notes
      !GroupNotesOn

      -- | Group separator
      (Maybe String)
  deriving (Show, Read, Eq)

-- | Supported grouping methods
data GroupNotesOn =
      GroupNotesOnId
    | GroupNotesOnDictInfos
    | GroupNotesOnFunctionIpe
    | GroupNotesOnLocationLabel
  deriving (Show, Read, Eq)

-- | Convert a 'GroupNotesOn' to an ordering function for use with 'sortBy'.
toComparison :: GroupNotesOn -> SpecialistNote -> SpecialistNote -> Ordering
toComparison =
    \case
      GroupNotesOnId -> compareId
      GroupNotesOnDictInfos -> compareDictInfos
      GroupNotesOnFunctionIpe -> compareFunctionIpe
      GroupNotesOnLocationLabel -> compareLocationLabel

-- | Parse the options for the @group@ command.
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

-- | Parse the grouping option
groupNotesOn :: Parser GroupNotesOn
groupNotesOn =
    option (maybeReader parseGroupNotesOn)
      (    long "on"
        <> help
             ( "Specify the trait to group the specialist notes on " ++
               "(\"id\", \"dict-info\", \"function-ipe\", or \"location-label\")"
             )
      )
  where
    parseGroupNotesOn :: String -> Maybe GroupNotesOn
    parseGroupNotesOn "id"             = Just GroupNotesOnId
    parseGroupNotesOn "dict-info"      = Just GroupNotesOnDictInfos
    parseGroupNotesOn "function-ipe"   = Just GroupNotesOnFunctionIpe
    parseGroupNotesOn "location-label" = Just GroupNotesOnLocationLabel
    parseGroupNotesOn x                = readMaybe x

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

groupNotes :: GroupNotesOptions -> [SpecialistNote] -> IO ()
groupNotes _                                []           = return ()
groupNotes (GroupNotesOptions groupOn msep) (note:notes) =
    foldM_ go note $ sortBy comparison notes
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
    comparison = toComparison groupOn
