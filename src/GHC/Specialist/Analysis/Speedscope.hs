module GHC.Specialist.Analysis.Speedscope where

import GHC.Specialist

import Data.Aeson
import Data.Maybe
import Data.List
import Data.Text qualified as Text
import GHC.RTS.Events
import HsSpeedscope
import System.FilePath
import Text.Read

speedscope :: Maybe FilePath -> Maybe String -> FilePath -> IO ()
speedscope mOutFile mDictLoc eventLogFile =
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          encodeFile (fromMaybe eventLogFileJSON mOutFile) $
            convertToSpeedscope
              (Nothing, Nothing)
              (const True)
              (processEvents mDictLoc)
              eventLog
        Left msg ->
          fail $ "failed to read event log: " ++ msg
  where
    eventLogFileJSON = takeFileName eventLogFile ++ ".json"

-- | Process specialist samples into a speedscope event log profile
processEvents :: Maybe String -> EventLogProfile -> Event -> EventLogProfile
processEvents mDictLoc elProf (Event _t ei _c) =
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
        let
          newProf =
            elProf
              { el_samples =
                    Sample
                      specialistNoteThreadId
                      specialistNoteCcIds
                  : el_samples elProf
              }
        in
          case mDictLoc of
            Just loc ->
              if
                any
                  (dictClosureContainsLoc loc . dictInfoClosure)
                  specialistNoteDictInfos
              then
                newProf
              else
                elProf
            Nothing ->
              newProf
      _ ->
        elProf
  where
    dictClosureContainsLoc loc (DictClosure mIpe frees) =
        case mIpe of
          Just ipe ->
            loc `isInfixOf` prettyInfoProv ipe
          Nothing ->
            False
        || any (dictClosureContainsLoc loc) frees