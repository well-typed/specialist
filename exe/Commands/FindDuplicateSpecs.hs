module Commands.FindDuplicateSpecs where

import GHC.Specialist.Analysis.DumpSpecialisations
import GHC.Specialist.Plugin.Orphans ()

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import GHC.Types.DumpSpecInfo
import Options.Applicative
import System.Directory.Recursive

data FindDuplicateSpecsOptions =
    FindDuplicateSpecsOptions
      { findDuplicateSpecsOptionsRootDir :: FilePath
      , findDuplicateSpecsOptionsExtension :: Maybe String
      }

-- | Parse the options for the @find-duplicate-specs@ command.
findDuplicateSpecsOptions :: Parser FindDuplicateSpecsOptions
findDuplicateSpecsOptions =
        FindDuplicateSpecsOptions
    <$> strOption
          (    long "root-dir"
            <> metavar "FILEPATH"
            <> help "Search files in this directory"
          )
    <*> optional
          ( strOption
              (    long "extension"
                <> metavar "EXTENSION"
                <> help
                     ( "Only search files with this suffix (default: " ++
                       "\".dump-specialisations\")"
                     )
              )
          )
    <**> helper

interpretFindDuplicateSpecsCommand
  :: FindDuplicateSpecsOptions
  -> IO ()
interpretFindDuplicateSpecsCommand FindDuplicateSpecsOptions{..} =
    printDuplicates
      =<< foldM findDuplicates Map.empty . filter hasExtension
      =<< getFilesRecursive findDuplicateSpecsOptionsRootDir
  where
    hasExtension :: FilePath -> Bool
    hasExtension = (extension `isSuffixOf`)

    extension :: String
    extension =
      fromMaybe ".dump-specialisations" findDuplicateSpecsOptionsExtension

    findDuplicates
      :: Map Text [DumpSpecInfo Text Text Text]
      -> FilePath
      -> IO (Map Text [DumpSpecInfo Text Text Text])
    findDuplicates acc =
      pure . foldl' go acc <=< readDumpSpecInfosFromFile

    go
      :: Map Text [DumpSpecInfo Text Text Text]
      -> DumpSpecInfo Text Text Text
      -> Map Text [DumpSpecInfo Text Text Text]
    go acc spec_info@DumpSpecInfo{..} =
      Map.insertWith
        (++)
        (dumpSpecInfo_polyId <> mconcat dumpSpecInfo_dicts)
        [spec_info]
        acc

    printDuplicates :: Map Text [DumpSpecInfo Text Text Text] -> IO ()
    printDuplicates =
      mapM_ printDupGroup . Map.filter ((>1) . length)

    printDupGroup :: [DumpSpecInfo Text Text Text] -> IO ()
    printDupGroup infos = do
      putStrLn "Duplicate group:"
      mapM_ (putStrLn . ("  " ++) . show) infos