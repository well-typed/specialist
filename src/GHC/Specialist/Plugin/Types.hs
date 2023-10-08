{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Plugin.Types where

import GHC.Specialist.Plugin.Orphans ()

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Kind
import Data.Map (Map)
import Data.Text (Text)
import GHC.InfoProv
import GHC.Plugins
import GHC.Types.DumpSpecInfo

-------------------------------------------------------------------------------
-- Instrumentation types
-------------------------------------------------------------------------------

data SpecialistNote =
    SpecialistNote
      { specialistNoteId :: String
      , specialistNoteCcs :: [String]
      , specialistNoteDictInfos :: [Maybe DictInfo]
      , specialistNoteFunctionIpe :: Maybe InfoProv
      , specialistNoteLocationLabel :: String
      , specialistNoteLocationSpan :: String
      }
  deriving (Show, Read, Eq)

-- This should probably just be derived in GHC
deriving instance Read InfoProv

data DictInfo =
    DictInfo
      { dictInfoProv :: Maybe InfoProv
      , dictInfoFreeDicts :: [DictInfo]
      }
  deriving (Show, Read, Eq, Ord)

data Dict (c :: Constraint) where
    Dict :: forall c. c => Dict c

-------------------------------------------------------------------------------
-- Monad definition
-------------------------------------------------------------------------------

data SpecialistEnv =
    SpecialistEnv
      { specialistEnvVerbosity :: !Verbosity
      , specialistEnvInputSpecsFile :: !FilePath
      , specialistEnvSampleProb :: !Double
      }

data SpecialistState =
    SpecialistState
      { specialistStateLastSourceNote :: !(Maybe (RealSrcSpan, String))
      , specialistStateUniqSupply :: UniqSupply
      , specialistStateInputSpecs :: Map Text (DumpSpecInfo Text Text Text)
      }

data Verbosity = Silent | Verbose | VeryVerbose

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
