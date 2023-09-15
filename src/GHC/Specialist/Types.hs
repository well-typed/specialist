{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Types where

import GHC.InfoProv

data SpecialistNote =
    SpecialistNote
      { specialistNoteId :: String
      , specialistNoteLocationLabel :: String
      , specialistNoteLocationSpan :: String
      , specialistNoteFunctionIpe :: Maybe InfoProv
      , specialistNoteInstanceIpe :: Maybe InfoProv
      }
  deriving (Show, Read, Eq)

-- This should probably just be derived in GHC
deriving instance Read InfoProv
