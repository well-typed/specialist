-- |
-- Functions for analyzing output from the specialist plugin.

module GHC.Specialist.Analysis
  ( -- * Analyzing event log output
    module GHC.Specialist.Analysis.EventLog

  , -- * Analyzing text files containing 'SpecialistNote's
    module GHC.Specialist.Analysis.TextFile

    -- * Comparing 'SpecialistNote's
  , eqById
  , eqByDictInfos
  , eqByFunctionIpe
  , eqByLocationLabel
  ) where

import GHC.Specialist.Analysis.EventLog
import GHC.Specialist.Analysis.TextFile
import GHC.Specialist.Plugin.Types

import Data.Maybe

-------------------------------------------------------------------------------
-- * Functions on 'SpecialistNote's
-------------------------------------------------------------------------------

eqById :: SpecialistNote -> SpecialistNote -> Bool
eqById (SpecialistNote cid1 _ _ _ _) (SpecialistNote cid2 _ _ _ _) =
    cid1 == cid2

eqByDictInfos :: SpecialistNote -> SpecialistNote -> Bool
eqByDictInfos (SpecialistNote _ dis1 _ _ _) (SpecialistNote _ dis2 _ _ _) =
    fromMaybe False ((==) <$> sequence dis1 <*> sequence dis2)

eqByFunctionIpe :: SpecialistNote -> SpecialistNote -> Bool
eqByFunctionIpe (SpecialistNote _ _ fIpe1 _ _) (SpecialistNote _ _ fIpe2 _ _) =
    fromMaybe False ((==) <$> fIpe1 <*> fIpe2)

eqByLocationLabel :: SpecialistNote -> SpecialistNote -> Bool
eqByLocationLabel (SpecialistNote _ _ _ l1 _) (SpecialistNote _ _ _ l2 _) =
    l1 == l2
