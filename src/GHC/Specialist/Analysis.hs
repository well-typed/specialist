-- |
-- Functions for analyzing output from the specialist plugin.

module GHC.Specialist.Analysis
  ( -- * Analyzing event log output
    module GHC.Specialist.Analysis.EventLog

    -- * Comparing 'SpecialistNote's
  , compareId
  , compareDictInfos
  , compareFunctionIpe
  , compareLocationLabel
  ) where

import GHC.Specialist.Analysis.EventLog
import GHC.Specialist.Plugin.Types

-------------------------------------------------------------------------------
-- * Functions on 'SpecialistNote's
-------------------------------------------------------------------------------

compareId :: SpecialistNote -> SpecialistNote -> Ordering
compareId (SpecialistNote cid1 _ _ _ _ _ _ _) (SpecialistNote cid2 _ _ _ _ _ _ _) =
    compare cid1 cid2

compareDictInfos :: SpecialistNote -> SpecialistNote -> Ordering
compareDictInfos (SpecialistNote _ _ _ dis1 _ _ _ _) (SpecialistNote _ _ _ dis2 _ _ _ _) =
    compare dis1 dis2

compareFunctionIpe :: SpecialistNote -> SpecialistNote -> Ordering
compareFunctionIpe (SpecialistNote _ _ _ _ fIpe1 _ _ _) (SpecialistNote _ _ _ _ fIpe2 _ _ _) =
    case (fIpe1, fIpe2) of
      (Nothing, Nothing) -> GT
      (fIpe1', fIpe2') -> compare fIpe1' fIpe2'

compareLocationLabel :: SpecialistNote -> SpecialistNote -> Ordering
compareLocationLabel (SpecialistNote _ _ _ _ _ l1 _ _) (SpecialistNote _ _ _ _ _ l2 _ _) =
    compare l1 l2
