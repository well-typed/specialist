module Commands.DictProvenance where

import Commands.Common
import Utils

import GHC.Specialist.Plugin.Types

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Options.Applicative

data DictProvenanceOptions =
    DictProvenanceOptions
      { dictProvenanceOptionsGetNotes :: IO (Either String [SpecialistNote])
      }

-- | Parse the options for the @dict-provenance@ command.
dictProvenanceOptions :: Parser DictProvenanceOptions
dictProvenanceOptions =
        DictProvenanceOptions
    <$> notesInput
    <**> helper

interpretDictProvenanceCommand :: DictProvenanceOptions -> IO ()
interpretDictProvenanceCommand DictProvenanceOptions{..} =
    dictProvenanceOptionsGetNotes >>=
      \case
        Right notes ->
          prettyPrint $ foldl' go Map.empty notes
        Left msg ->
          perish $ "failed to get specialist notes from the input: " <> msg
  where
    go
      :: Map DictInfo (Set [String])
      -> SpecialistNote
      -> Map DictInfo (Set [String])
    go acc note@SpecialistNote{..} =
      foldl' updateProvs acc (map (,note) $ catMaybes specialistNoteDictInfos)

    updateProvs
      :: Map DictInfo (Set [String])
      -> (DictInfo, SpecialistNote)
      -> Map DictInfo (Set [String])
    updateProvs acc (d,SpecialistNote{..})
      | null specialistNoteCcs =
          -- We have no call-stack data here, return
          acc
      | otherwise =
          -- We have a non-empty call-stack. Make sure we include information
          -- about the overloaded function itself in the call stack.
          let path = specialistNoteCcs ++ [show specialistNoteFunctionIpe] in
          case acc Map.!? d of
            Nothing ->
              -- This dictionary has not been seen, insert this path and return
              Map.insert d (Set.singleton path) acc
            Just paths ->
              -- We have recorded paths for this dictionary
              let
                -- Paths which the current path is prefixed by
                prefixedBy = Set.filter (`isPrefixOf` path) paths

                -- Paths which the current path is a prefix of, and paths which it
                -- is not a prefix of
                (prefixOf, notPrefixOf) = Set.partition (path `isPrefixOf`) paths
              in
                if not $ Set.null prefixedBy then
                  -- We have already tracked a prefix of the current path, return
                  acc
                else if not $ Set.null prefixOf then
                  -- The current path is a prefix of some of these paths, add this
                  -- path and remove the ones which it is a prefix of
                  Map.insert d (Set.insert path notPrefixOf) acc
                else
                  -- The current path is not a prefix of or prefixed by any current
                  -- paths, just add it
                  Map.insert d (Set.insert path paths) acc

    prettyPrint
      :: Map DictInfo (Set [String])
      -> IO ()
    prettyPrint result = do
      putStrLn "Dictionary provenances:"
      void $ Map.traverseWithKey prettyPrintDictProv result

    prettyPrintDictProv :: DictInfo -> Set [String] -> IO ()
    prettyPrintDictProv d paths = do
      putStrLn "  dictionary:"
      putStrLn $ "    " ++ show d
      putStrLn "  provenances:"
      mapM_
          prettyPrintCallStack
        . sortBy (\p1 p2 -> compare (length p1) (length p2))
        $ Set.elems paths
      putStrLn ""

    prettyPrintCallStack :: [String] -> IO ()
    prettyPrintCallStack ccs =
      putStrLn $ unlines $
          "    CC stack:"
        : map ("      " ++) ccs

