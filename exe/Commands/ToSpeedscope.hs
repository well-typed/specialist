{-# LANGUAGE OverloadedStrings #-}

module Commands.ToSpeedscope where

import Commands.Common
import GHC.Specialist
import Utils

import Data.Aeson
import Data.Maybe
-- import Data.Text (Text)
import Data.Text qualified as Text
-- import Data.Word (Word32)
import GHC.RTS.Events
-- import GHC.InfoProv
import HsSpeedscope
import Options.Applicative
import System.FilePath
import Text.Read

data ToSpeedscopeOptions =
    ToSpeedscopeOptions
      { toSpeedscopeOptionsEventLogFile :: FilePath
      , toSpeedscopeOptionsOutFile :: Maybe FilePath
      }

-- | Parse the options for the @to-speedscope@ command.
toSpeedscopeOptions :: Parser ToSpeedscopeOptions
toSpeedscopeOptions =
        ToSpeedscopeOptions
    <$> eventLogFileInput
    <*> optional
          ( strOption
              (    long "out"
                <> short 'o'
                <> metavar "FILE"
                <> help "Write the output JSON to this file path"
              )
          )
    <**> helper

interpretToSpeedscopeCommand :: ToSpeedscopeOptions -> IO ()
interpretToSpeedscopeCommand ToSpeedscopeOptions{..} =
    readEventLogFromFile toSpeedscopeOptionsEventLogFile >>=
      \case
        Right eventLog ->
          encodeFile (fromMaybe eventLogFileJSON toSpeedscopeOptionsOutFile) $
            convertToSpeedscope
              (Nothing, Nothing)
              (const True)
              processEvents
              eventLog
        Left msg ->
          perish $ "failed to read event log: " <> msg
  where
    eventLogFileJSON = takeFileName toSpeedscopeOptionsEventLogFile ++ ".json"

-- | Default processing function to convert profiling events into a classic speedscope
-- profile
processEvents :: EventLogProfile -> Event -> EventLogProfile
processEvents elProf (Event _t ei _c) =
  case ei of
    ProgramArgs _ (pname: _args) ->
      elProf { prog_name = Just pname }
    RtsIdentifier _ rts_ident ->
      elProf { rts_version = parseIdent rts_ident }
    ProfBegin ival ->
      elProf { prof_interval = Just ival }
    HeapProfCostCentre n l m s _ ->
      elProf { cost_centres = CostCentre n l m s : cost_centres elProf }
    UserMessage msg | Just (SpecialistNote {..}) <- readMaybe (Text.unpack msg) ->
      -- let
      --   custom_cc_id :: Word32
      --   custom_cc_id = fromIntegral . length $ cost_centres elProf

      --   custom_cc_id_str :: Text
      --   custom_cc_id_str = Text.pack $ show custom_cc_id

      --   custom_cc_name :: Text
      --   custom_cc_name =
      --       case specialistNoteFunctionIpe of
      --         Nothing -> "overloaded call #" <> custom_cc_id_str <> " (function IPE information unavailable)"
      --         Just InfoProv{..} -> Text.pack ipLabel
      -- in
        elProf
          { el_samples =
                Sample
                  specialistNoteThreadId
                  -- (custom_cc_id : specialistNoteCcIds)
                  specialistNoteCcIds
              : el_samples elProf
          -- , cost_centres =
          --       cost_centres elProf
          --     ++
          --       [ CostCentre
          --           custom_cc_id
          --           custom_cc_name
          --           ""
          --           (Text.pack $ show specialistNoteFunctionIpe)
          --       ]
          }
    _ ->
      elProf