-- | Ranking analysis

module GHC.Specialist.Analysis.Rank
  ( -- * Types
    RankMap
  , prettyRankMap

    -- * Logic
  , rank
  ) where

import GHC.Specialist.Plugin.Types

import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord
import GHC.InfoProv

-- | Generate a mapping from overloaded function and dictionary argument
-- combinations to call counts.
rank ::
     [String]
  -- ^ Omit functions whose IPE module includes any of these strings
  -> [String]
  -- ^ Include functions whose IPE module include any of these strings
  -> [SpecialistNote]
  -- ^ Plugin output
  -> RankMap
rank omitModules includeModules =
    foldl' go Map.empty
  where
    go :: RankMap -> SpecialistNote -> RankMap
    go acc SpecialistNote{..} =
        case specialistNoteFunctionIpe of
          Just fIpe ->
            if omitNote fIpe then
              acc
            else
              Map.insertWith (+) (fIpe, specialistNoteDictInfos) 1 acc
          Nothing ->
            acc
      where
        omitNote ipe =
               any (`isInfixOf` ipMod ipe) omitModules
            || (    not (null includeModules)
                 && not (any (`isInfixOf` ipMod ipe) includeModules)
               )

-- | Map from function IPE information and list of dictionary arguments to
-- number of samples emitted.
type RankMap = Map (InfoProv, [DictInfo]) Integer

-- | Pretty print a 'RankMap'
prettyRankMap :: RankMap -> String
prettyRankMap =
      foldr pretty ""
    . sortOn (Down . snd)
    . Map.toList
  where
    pretty :: ((InfoProv, [DictInfo]), Integer) -> String -> String
    pretty ((ip, dictInfos), count) acc =
        unlines
          ( [ prettyInfoProv ip
            , "  called " ++ show count ++ " times with dictionaries: "
            ] ++
              map (("    " ++) . prettyDictInfo) dictInfos
          ) ++
            acc
