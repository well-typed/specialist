{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans -ddump-simpl -ddump-to-file #-}

module GHC.Specialist.Plugin.Orphans where


-- import Data.Text
-- import GHC.Types.DumpSpecInfo
import GHC.InfoProv

-- deriving instance Eq (DumpSpecInfo Text Text Text)
-- deriving instance Ord (DumpSpecInfo Text Text Text)
-- deriving instance Show (DumpSpecInfo Text Text Text)
-- deriving instance Read (DumpSpecInfo Text Text Text)

deriving instance Ord InfoProv
