module Test.Specialist.Utils where

import GHC.Specialist

import Data.List
import GHC.InfoProv

-- | Find the first element of a list satisfying a predicate and return that
-- element and the list with the element removed.
findAndRemove :: Eq a => (a -> Bool) -> [a] -> Maybe (a, [a])
findAndRemove _ [] = Nothing
findAndRemove p (x:xs)
    | p x = Just (x, xs)
    | otherwise = fmap (x:) <$> findAndRemove p xs

-- | Checks whether at least one of the dictionary infos in the note references
-- a dictionary whose label string contains the given string.
includesDictLabel :: String -> SpecialistNote -> Bool
includesDictLabel s SpecialistNote{..} =
  any (dictIncludesDictLabel s) specialistNoteDictInfos

-- | Checks whether at least one of the dictionary infos in the note references
-- a dictionary whose name string contains the given string.
includesDictName :: String -> SpecialistNote -> Bool
includesDictName s SpecialistNote{..} =
  any (dictIncludesDictName s) specialistNoteDictInfos

-- | Checks whether the IPE label of the 'DictInfo' contains the given string.
dictIncludesDictLabel :: String -> DictInfo -> Bool
dictIncludesDictLabel s =
  maybe False ((s `isInfixOf`) . ipLabel) . dictInfoProv

-- | Checks whether the IPE name of the 'DictInfo' contains the given string.
dictIncludesDictName :: String -> DictInfo -> Bool
dictIncludesDictName s =
  maybe False ((s `isInfixOf`) . ipName) . dictInfoProv
