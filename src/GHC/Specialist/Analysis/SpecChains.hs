-- | "Specialization chain" analysis
module GHC.Specialist.Analysis.SpecChains
  ( -- * Types
    SpecChains
  , prettySpecChainMap

    -- * Logic
  , specChains
  ) where

import GHC.Specialist.Plugin.Types

import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.InfoProv

-- | Generate a mapping from overloaded function and dictionary argument
-- combinations to call counts.
specChains ::
     Maybe String
  -- ^ Only track dictionaries whose prettified IPE info contains this
  -- substring
  -> Maybe String
  -- ^ Only produce chains including at least one cost center label containing
  -- this substring
  -> [SpecialistNote]
  -- ^ Plugin output
  -> SpecChains
specChains mDictFilt mCCFilt notes =
    let
      callMap :: CallMap
      callMap = foldl' goDictMap Map.empty notes
    in
      removeInfixes $ foldl' (goSpecChains callMap) Map.empty notes
  where
    -- Add the function and dictionaries involved in the note to the 'CallMap'
    --
    -- If we don't have IPE info for the function we can't identify it so we
    -- can't do anything
    --
    -- If we do, only add the first dict closure from the call that contains the
    -- dictionary filter string
    goDictMap :: CallMap -> SpecialistNote -> CallMap
    goDictMap acc SpecialistNote{..} =
        case specialistNoteFunctionIpe of
          Just fIpe ->
            foldl' (go fIpe) acc specialistNoteDictInfos
          Nothing ->
            acc
      where
        go :: InfoProv -> CallMap -> DictInfo -> CallMap
        go fIpe acc' di
            | Just dIpe <- keepDict di
            = Map.insertWith
                (\(c1, d1) (c2, d2) -> (c1 + c2, Set.union d1 d2))
                (ipeToCCPrefix fIpe)
                (1, Set.singleton $ prettyInfoProv dIpe)
                acc'
              -- TODO: Change to
              -- Map.alter
              --   _
              --   addCall
              --   acc'
            | otherwise
            = acc'
          where
            keepDict :: DictInfo -> Maybe InfoProv
            keepDict
                | Just dictFilt <- mDictFilt
                = find
                    ( (dictFilt `isInfixOf`)
                    . prettyInfoProv
                    )
                    . dictClosureIpes
                    . dictInfoClosure
                | otherwise
                = listToMaybe . dictClosureIpes . dictInfoClosure

            ipeToCCPrefix :: InfoProv -> String
            ipeToCCPrefix InfoProv{..} = ipMod ++ "." ++ ipLabel

    goSpecChains :: CallMap -> SpecChains -> SpecialistNote -> SpecChains
    goSpecChains calls acc SpecialistNote{..}
        | Just ccFilt <- mCCFilt
        , not $ any (ccFilt `isInfixOf`) specialistNoteCcs
        = acc
        | otherwise
        = newChains
      where
        newChains = foldl' addChains acc $ chains Nothing specialistNoteCcs

        addChains :: SpecChains -> ([String], (Integer, Set String)) -> SpecChains
        addChains accChains (chain, count)
            | Just ccFilt <- mCCFilt
            , not $ any (ccFilt `isInfixOf`) chain
            = accChains
            | otherwise
            = Map.insert chain count accChains

        chains ::
              Maybe ([String], (Integer, Set String))
          -- ^ Accumulated chain and count of calls so far paired with the
          -- dictionaries that matched at those nodes
          -> [String]
          -- ^ Remaining cost center stack
          -> [([String], (Integer, Set String))]
        chains Nothing =
            \case
              [] ->
                []
              (cc:ccs) | Just countAndDs <- calledAt cc ->
                chains (Just ([cc], countAndDs)) ccs
              (_:ccs) ->
                chains Nothing ccs
        chains (Just (accChain, (!accCount, !accDs))) =
            \case
              [] ->
                [(reverse accChain, (accCount, accDs))]
              (cc:ccs) | Just (count, ds) <- calledAt cc ->
                chains (Just (cc:accChain, (count + accCount, ds `Set.union` accDs))) ccs
              (_:ccs) ->
                (reverse accChain, (accCount, accDs)) : chains Nothing ccs

        -- We assume that the cost center labels starts with "Module.label"
        -- followed by a space, and attempt to match that on the stored function
        -- information
        calledAt :: String -> Maybe (Integer, Set String)
        calledAt =
            (`Map.lookup` calls) . ccToCCPrefix
          where
            ccToCCPrefix = takeWhile (/= ' ')

    removeInfixes :: SpecChains -> SpecChains
    removeInfixes =
        Map.fromList . filt . Map.toAscList
      where
        filt ::
             [([String], (Integer, Set String))]
          -> [([String], (Integer, Set String))]
        filt chains =
            foldr go [] chains
          where
            go ::
                 ([String], (Integer, Set String))
              -> [([String], (Integer, Set String))]
              -> [([String], (Integer, Set String))]
            go (ch,ct) acc
                | any (\(ch',_) -> ch /= ch' && ch `isInfixOf` ch') chains
                = acc
                | otherwise
                = (ch,ct):acc

-- | Maps chains of overloaded calls to the dictionaries that were passed
-- through the chain.
--
-- The dictionaries are paired with the total number of samples that came from
-- the functions in the chain.
type SpecChains = Map [String] (Integer, Set String)

-- | Maps functions to the set of dictionaries they were called with, either
-- directly or as a superclass.
--
-- The dictionaries are represented as their prettified IPE strings. Each
-- dictionary contains the dictionary filter string as a substring. The set of
-- dictionaries is also paired with the number of samples we saw where the
-- function called with those the dictionaries.
--
-- The functions are represented as their CC label string (Module.label) so we
-- can look them up easier later.
type CallMap = Map String (Integer, Set String)

-- | Pretty print a 'SpecChainMap'
prettySpecChainMap :: SpecChains -> String
prettySpecChainMap =
      foldr pretty ""
    . sortOn (Down . fst . snd)
    . Map.toList
  where
    pretty :: ([String], (Integer, Set String)) -> String -> String
    pretty (chain, (count, ds)) acc =
        unlines
          ( "matching dictionaries: "
            : map ("    " ++) (Set.toList ds)
              ++
              [ "  included in " ++ show count ++ " total calls through chain:"

              ]
              ++
              map ("    " ++) chain
          ) ++
            acc
