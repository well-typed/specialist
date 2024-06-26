module GHC.Specialist.Plugin.Initialization where

import GHC.Specialist.Plugin.Types

import Data.Char qualified as Char
import Data.Maybe
import Data.Set qualified as Set
import GHC.Plugins
import GHC.Types.CostCentre.State
import GHC.Utils.Logger
import Text.Read (readMaybe)

defaultSpecialistEnv :: HscEnv -> SpecialistEnv
defaultSpecialistEnv hsc_env =
    SpecialistEnv
      { specialistEnvVerbosity =
          Silent
      , specialistEnvSampleProb =
          0.01
      , specialistEnvHscEnv =
          hsc_env
      , specialistEnvCostCenters =
          False
      }

-- | Parse the plugin options into a 'SpecialistEnv'
mkSpecialistEnv :: HscEnv -> [CommandLineOption] -> IO SpecialistEnv
mkSpecialistEnv hsc_env opts = do
    let init_env = defaultSpecialistEnv hsc_env
    pure $ foldl' parseOpts init_env opts
  where
    parseOpts :: SpecialistEnv -> CommandLineOption -> SpecialistEnv
    parseOpts env opt =
        case map Char.toLower opt of
          -- An occurrence of "v" means the user has requested verbose output
          "v" ->
            env { specialistEnvVerbosity = Verbose }

          -- An occurrence of "vv" means the user has requested very verbose
          -- output
          "vv" ->
            env { specialistEnvVerbosity = VeryVerbose }

          -- An occurrence of "f:X" where X parses as a Double means the user
          -- has requested a sample probability of X (e.g. 0.01 means 1% sample
          -- probability)
          'f':':':freqStr | Just freq <- readMaybe freqStr ->
            env { specialistEnvSampleProb = freq }

          -- An occurrence of "ccs" means the user has requested cost center
          -- insertion
          "ccs" ->
            env { specialistEnvCostCenters = True }

          unknown ->
            error $ "specialist: unknown argument: " ++ unknown

logFlagsToDumpSpecsFile :: LogFlags -> FilePath
logFlagsToDumpSpecsFile log_flags =
    logFlagsToDumpPrefix log_flags ++ "dump-specialisations"

logFlagsToDumpPrefix :: LogFlags -> String
logFlagsToDumpPrefix LogFlags{..} =
   fromMaybe log_dump_prefix log_dump_prefix_override

initSpecialistState
  :: Module
  -> CostCentreState
  -> IO SpecialistState
initSpecialistState curMod cc_state = do
    uniqSupply <- mkSplitUniqSupply 'z'
    return $
      SpecialistState
        { specialistStateLastSourceNote = Nothing
        , specialistStateCurrentModule = Just curMod
        , specialistStateLocalCcs = Set.empty
        , specialistStateCostCentreState = cc_state
        , specialistStateUniqSupply = uniqSupply
        -- , specialistStateInputSpecs = input_specs
        , specialistStateOverloadedCallCount = 0
        , specialistStateIORefBinds = []
        }

-- | Reads the 'DumpSpecInfo's from the dump file at the given file path and
-- creates a map indexed by the name of the function that has a specialisation
-- generated.
--
-- TODO: Not merged in GHC.
{-
readDumpSpecInfosToMap
  :: MonadIO m
  => SpecialistEnv
  -> FilePath
  -> m (Map Text (DumpSpecInfo Text Text Text))
readDumpSpecInfosToMap env dump_file = liftIO $ do
    exists <- doesFileExist dump_file
    if exists then do
      slogVIO env $ "Reading specialisations from file: " ++ dump_file
      -- TODO: Would be interesting to flesh this out more. What sort of things
      -- could the plugin do if it knew what specializations were already
      -- generated in this module?
      specs <- pure Map.empty -- foldr go Map.empty <$> readDumpSpecInfosFromFile dump_file
      slogVVIO env "Read input specialisations:"
      slogVVIO env $ show specs
      return specs
    else
      pure Map.empty
  where
    _go
      :: DumpSpecInfo Text Text Text
      -> Map Text (DumpSpecInfo Text Text Text)
      -> Map Text (DumpSpecInfo Text Text Text)
    _go info@DumpSpecInfo{..} = Map.insert dumpSpecInfo_polyId info
-}