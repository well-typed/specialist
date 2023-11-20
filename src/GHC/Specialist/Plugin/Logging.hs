{-# LANGUAGE UndecidableInstances #-}

-- |
-- Utility functions for logging from the plugin.

module GHC.Specialist.Plugin.Logging where

import GHC.Specialist.Plugin.Types

import Control.Monad.Reader
import GHC.Plugins

class SpecialistLoggable a where
  slog :: SpecialistEnv -> a -> IO ()

instance {-# OVERLAPPING #-} SpecialistLoggable String where
  slog _ = putStrLn

instance Outputable a => SpecialistLoggable a where
  slog env@SpecialistEnv{..} =
    slog env . showPpr (hsc_dflags specialistEnvHscEnv)

-------------------------------------------------------------------------------
-- * Logging functions
-------------------------------------------------------------------------------

-- | Log only if very verbose output is enabled
slogVV :: SpecialistLoggable a => a -> SpecialistM ()
slogVV x = do
    env <- ask
    lift $ slogVVIO env x

-- | Log only if very verbose output is enabled
slogV :: SpecialistLoggable a => a -> SpecialistM ()
slogV x = do
    env <- ask
    lift $ slogVIO env x

-- | Log only if very verbose output is enabled, in the 'IO' monad.
slogVVIO :: SpecialistLoggable a => SpecialistEnv -> a -> IO ()
slogVVIO env@SpecialistEnv{..} x =
    case specialistEnvVerbosity of
      VeryVerbose -> slog env x
      Verbose -> return ()
      Silent -> return ()

-- | Log only if verbose output is enabled, in the 'IO' monad.
slogVIO :: SpecialistLoggable a => SpecialistEnv -> a -> IO ()
slogVIO env@SpecialistEnv{..} x =
    case specialistEnvVerbosity of
      VeryVerbose -> slog env x
      Verbose -> slog env x
      Silent -> return ()

-- | Log no matter the verbosity, in the 'IO' monad.
slogIO :: SpecialistLoggable a => SpecialistEnv -> a -> IO ()
slogIO = slog
