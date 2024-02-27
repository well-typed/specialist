{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Plugin.Types where

import GHC.Specialist.Plugin.Orphans ()

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Kind
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import GHC.InfoProv
import GHC.Plugins
import GHC.Tc.Utils.Env
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.DumpSpecInfo
import GHC.Types.TyThing

-------------------------------------------------------------------------------
-- Instrumentation types
-------------------------------------------------------------------------------

data SpecialistNote =
    SpecialistNote
      { specialistNoteId :: String
      , specialistNoteCcs :: [String]
      , specialistNoteCcIds :: [Word32]
      , specialistNoteDictInfos :: [DictInfo]
      , specialistNoteFunctionIpe :: Maybe InfoProv
      , specialistNoteLocationLabel :: String
      , specialistNoteLocationSpan :: String
      , specialistNoteThreadId :: !Word32
      }
  deriving (Show, Read, Eq, Ord)

prettyPrint :: SpecialistNote -> String
prettyPrint SpecialistNote{..} =
    "Note " ++ specialistNoteId ++ ": \n" ++
    "  function IPE:            " ++ show specialistNoteFunctionIpe ++ "\n" ++
    "  location label and span: (" ++ show specialistNoteLocationLabel ++
      ", " ++ show specialistNoteLocationSpan ++ ")\n" ++
    "  dictionary ipes: \n" ++
      unlines (map (("    " ++) . show) specialistNoteDictInfos)

-- This should probably just be derived in GHC
deriving instance Read InfoProv

data DictInfo =
    DictInfo
      { dictInfoType :: String
      , dictInfoClosure :: DictClosure
      }
  deriving (Show, Read, Eq, Ord)

data DictClosure =
    DictClosure
      { dictClosureProv :: Maybe InfoProv
      , dictClosureFreeDicts :: [DictClosure]
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
      , specialistEnvHscEnv :: !HscEnv
      , specialistEnvCostCenters :: !Bool
      }

data SpecialistState =
    SpecialistState
      { specialistStateLastSourceNote :: !(Maybe (RealSrcSpan, String))
      , specialistStateCurrentModule :: !(Maybe Module)
      , specialistStateLocalCcs :: !(Set CostCentre)
      , specialistStateCostCentreState :: CostCentreState
      , specialistStateUniqSupply :: UniqSupply
      , specialistStateInputSpecs :: !(Map Text (DumpSpecInfo Text Text Text))
      , specialistStateOverloadedCallCount :: !Integer
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

instance MonadIO m => MonadThings (SpecialistT m) where
  lookupThing name = do
    hsc_env <- asks specialistEnvHscEnv
    liftIO $ lookupGlobal hsc_env name

instance Monad m => HasDynFlags (SpecialistT m) where
  getDynFlags = asks (hsc_dflags . specialistEnvHscEnv)

mkCcIndex :: Monad m => FastString -> SpecialistT m CostCentreIndex
mkCcIndex nm = do
    s@SpecialistState{..} <- get
    let (ix, ccs) = getCCIndex nm specialistStateCostCentreState
    put s { specialistStateCostCentreState = ccs }
    return ix

type SpecialistM = SpecialistT IO

evalSpecialist
  :: Monad m
  => SpecialistEnv
  -> SpecialistState
  -> SpecialistT m a
  -> m a
evalSpecialist env s (SpecialistT run) = evalStateT (runReaderT run env) s

runSpecialist
  :: Monad m
  => SpecialistEnv
  -> SpecialistState
  -> SpecialistT m a
  -> m (a, SpecialistState)
runSpecialist env s (SpecialistT run) = runStateT (runReaderT run env) s
