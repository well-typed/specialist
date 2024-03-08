module Test.Specialist.Utils where

import GHC.Specialist

import Control.Monad
import Data.List
import GHC.InfoProv
import Test.Tasty.HUnit


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
    any (dictIncludesDictLabel s . dictInfoClosure) specialistNoteDictInfos

-- | Checks whether at least one of the dictionary infos in the note references
-- a dictionary whose name string contains the given string.
includesDictName :: String -> SpecialistNote -> Bool
includesDictName s SpecialistNote{..} =
    any (dictIncludesDictName s . dictInfoClosure) specialistNoteDictInfos

-- | Checks whether the IPE label of the 'DictClosure' contains the given string.
dictIncludesDictLabel :: String -> DictClosure -> Bool
dictIncludesDictLabel s =
    maybe False ((s `isInfixOf`) . ipLabel) . dictClosureIpe

-- | Checks whether the IPE name of the 'DictClosure' contains the given string.
dictIncludesDictName :: String -> DictClosure -> Bool
dictIncludesDictName s =
    maybe False ((s `isInfixOf`) . ipName) . dictClosureIpe

-- | Check that the dictionaries referenced in the note abide by some basic
-- superclass relationships
checkBasicSuperclasses :: SpecialistNote -> Assertion
checkBasicSuperclasses SpecialistNote{..} =
    mapM_ (checkBasicDictSuperclasses . dictInfoClosure) specialistNoteDictInfos

-- | Check that the dictionaries referenced in the note abide by some basic
-- superclass relationships from the Prelude in addition to those given.
checkSuperclasses :: [(String, String)] -> SpecialistNote -> Assertion
checkSuperclasses rs SpecialistNote{..} =
    mapM_ (checkDictSuperclasses rs . dictInfoClosure) specialistNoteDictInfos

-- | Check that the dictionaries referenced in the note abide by the given
-- superclass relationships.
checkSuperclassesWith :: [(String, String)] -> SpecialistNote -> Assertion
checkSuperclassesWith rs SpecialistNote{..} =
    mapM_ (checkDictSuperclassesWith rs . dictInfoClosure) specialistNoteDictInfos

-- | Check some basic superclass relationships for the given dictionary and it's
-- superclasses
checkBasicDictSuperclasses :: DictClosure -> Assertion
checkBasicDictSuperclasses = checkDictSuperclasses []

-- | Check that the given superclass relationships in addition to some basic
-- Prelude class relationships hold for the given dictionary and it's
-- superclasses.
checkDictSuperclasses :: [(String, String)] -> DictClosure -> Assertion
checkDictSuperclasses =
    checkDictSuperclassesWith .
      (
        [ ("C:Ord", "C:Eq")
        , ("C:Real", "C:Num")
        , ("C:Real", "C:Ord")
        , ("C:Integral", "C:Real")
        , ("C:Integral", "C:Enum")
        ] ++
      )

-- | Check that the given superclass relationships hold for the given
-- dictionary.
checkDictSuperclassesWith
  :: [(String, String)]
  -- ^ Assoc. list mapping classes to their expected superclasses
  -> DictClosure
  -- ^ 'DictClosure' to check
  -> Assertion
checkDictSuperclassesWith rs dc = do
    mapM_ checkGivenRel rs
    mapM_ (checkDictSuperclassesWith rs) (dictClosureFrees dc)
  where
    checkGivenRel (c, sc) =
      when (dictIncludesDictName c dc) $
        any (dictIncludesDictName sc) (dictClosureFrees dc) @?
          "expect " ++ sc ++ " to be a superclass of " ++ c
