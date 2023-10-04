{-# LANGUAGE UndecidableInstances #-}

-- |
-- Utility functions for logging from the plugin.

module GHC.Specialist.Plugin.Logging where

import GHC.Specialist.Plugin.Types

import Control.Monad.Reader
import GHC.Plugins

class CoreMLoggable a where
  coreMLog :: a -> CoreM ()

instance {-# OVERLAPPING #-} CoreMLoggable String where
  coreMLog = putMsgS

instance Outputable a => CoreMLoggable a where
  coreMLog = putMsg . ppr

-------------------------------------------------------------------------------
-- * Logging functions
-------------------------------------------------------------------------------

-- | Log only if very verbose output is enabled
slogVV :: CoreMLoggable a => a -> SpecialistM ()
slogVV x = do
    env <- ask
    lift $ slogCoreVV env x

-- | Log only if very verbose output is enabled
slogV :: CoreMLoggable a => a -> SpecialistM ()
slogV x = do
    env <- ask
    lift $ slogCoreV env x

-- | Log no matter the verbosity
slog :: CoreMLoggable a => a -> SpecialistM ()
slog = lift . slogCore

-- | Log only if very verbose output is enabled, in the 'CoreM' monad.
slogCoreVV :: CoreMLoggable a => SpecialistEnv -> a -> CoreM ()
slogCoreVV SpecialistEnv{..} x =
    case specialistEnvVerbosity of
      VeryVerbose -> coreMLog x
      Verbose -> return ()
      Silent -> return ()

-- | Log only if verbose output is enabled, in the 'CoreM' monad.
slogCoreV :: CoreMLoggable a => SpecialistEnv -> a -> CoreM ()
slogCoreV SpecialistEnv{..} x =
    case specialistEnvVerbosity of
      VeryVerbose -> coreMLog x
      Verbose -> coreMLog x
      Silent -> return ()

-- | Log no matter the verbosity, in the 'CoreM' monad.
slogCore :: CoreMLoggable a => a -> CoreM ()
slogCore = coreMLog
