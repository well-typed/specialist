{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Types where

import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.InfoProv
import GHC.Plugins

-------------------------------------------------------------------------------
-- Instrumentation types
-------------------------------------------------------------------------------

data SpecialistNote =
    SpecialistNote
      { specialistNoteId :: String
      , specialistNoteLocationLabel :: String
      , specialistNoteLocationSpan :: String
      , specialistNoteFunctionIpe :: Maybe InfoProv
      , specialistNoteInstanceIpe :: [Maybe InfoProv]
      }
  deriving (Show, Read, Eq)

-- This should probably just be derived in GHC
deriving instance Read InfoProv

-------------------------------------------------------------------------------
-- Monad definition
-------------------------------------------------------------------------------

data SpecialistEnv =
    SpecialistEnv
      { specialistEnvVerbosity :: !Verbosity
      }

data SpecialistState =
    SpecialistState
      { specialistStateLastSourceNote :: !(Maybe (RealSrcSpan, String))
      , specialistStateUniqSupply :: UniqSupply
      }

initSpecialistState :: CoreM SpecialistState
initSpecialistState = do
    uniqSupply <- liftIO $ mkSplitUniqSupply 'z'
    return $
      SpecialistState
        { specialistStateLastSourceNote = Nothing
        , specialistStateUniqSupply = uniqSupply
        }

data Verbosity = Silent | Verbose

newtype SpecialistT m a =
    SpecialistT
      { runSpecialistT :: ReaderT SpecialistEnv (StateT SpecialistState m) a
      }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SpecialistEnv
    , MonadState SpecialistState
    )

deriving instance (MonadIO m) => MonadIO (SpecialistT m)

instance MonadTrans SpecialistT where
  lift act = SpecialistT (lift $ lift act)

instance Monad m => MonadUnique (SpecialistT m) where
  getUniqueSupplyM = do
    (s1, s2) <- splitUniqSupply <$> gets specialistStateUniqSupply
    modify $
      \st -> st { specialistStateUniqSupply = s2 }
    return s1

  getUniqueM = do
    (u, s) <- takeUniqFromSupply <$> gets specialistStateUniqSupply
    modify $
      \st -> st { specialistStateUniqSupply = s }
    return u

type SpecialistM = SpecialistT CoreM

runSpecialist
  :: Monad m
  => SpecialistEnv
  -> SpecialistState
  -> SpecialistT m a
  -> m a
runSpecialist env s (SpecialistT run) = evalStateT (runReaderT run env) s
