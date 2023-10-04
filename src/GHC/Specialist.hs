
module GHC.Specialist
  ( -- * Plugin definition
    module GHC.Specialist.Plugin
    -- **  Types
  , module GHC.Specialist.Plugin.Types

    -- * Plugin output analysis
  , module GHC.Specialist.Analysis
  ) where

import GHC.Specialist.Analysis
import GHC.Specialist.Plugin
import GHC.Specialist.Plugin.Types
