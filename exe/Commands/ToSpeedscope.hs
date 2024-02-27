{-# LANGUAGE OverloadedStrings #-}

module Commands.ToSpeedscope where

import GHC.Specialist

import Data.Aeson
import Data.Maybe
import Data.Text qualified as Text
import GHC.RTS.Events
import HsSpeedscope
import Options.Applicative
import System.FilePath
import Text.Read

data ToSpeedscopeOptions =
    ToSpeedscopeOptions
      { toSpeedscopeOptionsOutFile :: Maybe FilePath
      }
  deriving (Show, Read, Eq)

-- | Parse the options for the @to-speedscope@ command.
toSpeedscopeOptions :: Parser ToSpeedscopeOptions
toSpeedscopeOptions =
        ToSpeedscopeOptions
    <$> optional
          ( strOption
              (    long "out"
                <> short 'o'
                <> metavar "FILE"
                <> help "Write the output JSON to this file path"
              )
          )
    <**> helper

toSpeedscope :: ToSpeedscopeOptions -> FilePath -> IO ()
toSpeedscope ToSpeedscopeOptions{..} eventLogFile =
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          encodeFile (fromMaybe eventLogFileJSON toSpeedscopeOptionsOutFile) $
            convertToSpeedscope
              (Nothing, Nothing)
              (const True)
              processEvents
              eventLog
        Left msg ->
          fail $ "failed to read event log: " <> msg
  where
    eventLogFileJSON = takeFileName eventLogFile ++ ".json"

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
        elProf
          { el_samples =
                Sample
                  specialistNoteThreadId
                  specialistNoteCcIds
              : el_samples elProf
          }
    _ ->
      elProf