module GHC.Specialist.Plugin.Initialization where

import GHC.Specialist.Analysis.DumpSpecialisations
import GHC.Specialist.Plugin.Logging
import GHC.Specialist.Plugin.Types

import Control.Monad.IO.Class
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import GHC.Plugins
import GHC.Types.DumpSpecInfo
import GHC.Utils.Logger
import System.Directory
import Text.Read (readMaybe)

defaultSpecialistEnv :: LogFlags -> SpecialistEnv
defaultSpecialistEnv log_flags =
    SpecialistEnv
      { specialistEnvVerbosity = Silent
      , specialistEnvInputSpecsFile = logFlagsToDumpSpecsFile log_flags
      , specialistEnvSampleProb = 0.01
      }

-- | Parse the plugin options into a 'SpecialistEnv'
mkSpecialistEnv :: [CommandLineOption] -> CoreM SpecialistEnv
mkSpecialistEnv opts = do
    log_flags <- logFlags <$> getLogger
    let init_env = defaultSpecialistEnv log_flags
    pure $ foldl' parseOpts init_env opts
  where
    parseOpts :: SpecialistEnv -> CommandLineOption -> SpecialistEnv
    parseOpts env = \case
      -- An occurrence of "v" means the user has requested verbose output
      "v" ->
        env { specialistEnvVerbosity = Verbose }

      -- An occurrence of "vv" means the user has requested very verbose output
      "vv" ->
        env { specialistEnvVerbosity = VeryVerbose }

      -- An occurrence of "f:X" where X parses as a Double means the user has
      -- requests a sample probability of X (e.g. 0.01 means 1% sample
      -- probability)
      'f':':':freqStr | Just freq <- readMaybe freqStr ->
        env { specialistEnvSampleProb = freq }

      -- Any other argument means the user is overriding the location of dump
      -- output to read for this module
      file ->
        env { specialistEnvInputSpecsFile = file }

logFlagsToDumpSpecsFile :: LogFlags -> FilePath
logFlagsToDumpSpecsFile log_flags =
    logFlagsToDumpPrefix log_flags ++ "dump-specialisations"

logFlagsToDumpPrefix :: LogFlags -> String
logFlagsToDumpPrefix LogFlags{..} =
   fromMaybe log_dump_prefix log_dump_prefix_override

initSpecialistState :: SpecialistEnv -> CoreM SpecialistState
initSpecialistState env = do
    let input_specs_file = specialistEnvInputSpecsFile env
    slogCoreV env $ "Reading specialisations from file: " ++ input_specs_file
    input_specs <- liftIO $ readDumpSpecInfosToMap input_specs_file
    slogCoreVV env "Read input specialisations:"
    slogCoreVV env $ show input_specs
    uniqSupply <- liftIO $ mkSplitUniqSupply 'z'
    return $
      SpecialistState
        { specialistStateLastSourceNote = Nothing
        , specialistStateUniqSupply = uniqSupply
        , specialistStateInputSpecs = input_specs
        }

-- | Reads the 'DumpSpecInfo's from the dump file at the given file path and
-- creates a map indexed by the name of the function that has a specialisation
-- generated.
readDumpSpecInfosToMap
  :: MonadIO m
  => FilePath
  -> m (Map Text (DumpSpecInfo Text Text Text))
readDumpSpecInfosToMap dump_file = liftIO $ do
    dumpFileExists <- doesFileExist dump_file
    if dumpFileExists then do
      foldr go Map.empty <$> readDumpSpecInfosFromFile dump_file
    else
      pure Map.empty
  where
    go
      :: DumpSpecInfo Text Text Text
      -> Map Text (DumpSpecInfo Text Text Text)
      -> Map Text (DumpSpecInfo Text Text Text)
    go info@DumpSpecInfo{..} = Map.insert dumpSpecInfo_polyId info
