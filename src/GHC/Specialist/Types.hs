{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Types where

import GHC.InfoProv

data SpecialistNote =
    SpecialistNote
      { specialistNoteFunctionInfoProv :: Maybe InfoProv
      , specialistNoteInstanceInfoProv :: Maybe InfoProv
      , specialistNoteMeta :: String
      }
  deriving (Show, Read, Eq)

-- This should probably just be derived in GHC
deriving instance Read InfoProv
