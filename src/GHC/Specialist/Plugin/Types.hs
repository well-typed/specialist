{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Specialist.Plugin.Types where

import GHC.Specialist.Plugin.Orphans ()

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Kind
-- Only imported for unimplemented DumpSpecInfo tracking
-- import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
-- Only imported for unimplemented DumpSpecInfo tracking
-- import Data.Text (Text)
import Data.Word
import GHC.Internal.InfoProv
import GHC.Internal.ClosureTypes
import GHC.Plugins
import GHC.Tc.Utils.Env
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
-- import GHC.Types.DumpSpecInfo
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

prettySpecialistNote :: SpecialistNote -> String
prettySpecialistNote SpecialistNote{..} =
    "Note " ++ specialistNoteId ++ ":\n  call to:\n"
    ++
    case specialistNoteFunctionIpe of
      Just ipe ->
        "    " ++ prettyInfoProv ipe ++ "\n"
      Nothing ->
        "    <unknown>\n"
    ++
    "  with dictionaries:\n"
    ++
    unlines (map (("    " ++) . prettyDictInfo) specialistNoteDictInfos)

prettyInfoProv :: InfoProv -> String
prettyInfoProv ipe  =
    prettyInfoProvModLabel ipe ++ " (" ++ prettyInfoProvFileSpan ipe ++ ")"

prettyInfoProvModLabel :: InfoProv -> String
prettyInfoProvModLabel InfoProv{..} = ipMod ++ "." ++ ipLabel

prettyInfoProvFileSpan :: InfoProv -> String
prettyInfoProvFileSpan InfoProv{..} = ipSrcFile ++ ":" ++ ipSrcSpan

-- This should probably just be derived in GHC
deriving instance Read ClosureType
deriving instance Read InfoProv

data DictInfo =
      DictInfo
        { dictInfoType :: String
        , dictInfoClosure :: DictClosure
        }
  deriving (Show, Read, Eq, Ord)

prettyDictInfo :: DictInfo -> String
prettyDictInfo (DictInfo t c) =
    maybe "<no IPE data>" prettyInfoProv (dictClosureIpe c)
    ++
    " (" ++ t ++ ")"

data DictClosure =
      DictClosure (Maybe InfoProv) [DictClosure]
    | DictClosureRaw
        (Maybe InfoProv)
        -- | Shown 'GHC.Heap.Exts.Closure'
        String

  deriving (Show, Read, Eq, Ord)

dictClosureIpe :: DictClosure -> Maybe InfoProv
dictClosureIpe =
    \case
      DictClosure ipe    _ -> ipe
      DictClosureRaw ipe _ -> ipe

dictClosureFrees :: DictClosure -> [DictClosure]
dictClosureFrees (DictClosure _ frees) = frees
dictClosureFrees (DictClosureRaw _ _) = []

dictClosureIpes :: DictClosure -> [InfoProv]
dictClosureIpes (DictClosure mIpe frees) =
    case mIpe of
      Just ipe ->
        (ipe :)
      Nothing ->
        id
    $ concatMap dictClosureIpes frees
dictClosureIpes (DictClosureRaw mIpe _) =
    maybeToList mIpe

data Dict (c :: Constraint) where
    Dict :: forall c. c => Dict c

-------------------------------------------------------------------------------
-- Monad definition
-------------------------------------------------------------------------------

data SpecialistEnv =
    SpecialistEnv
      { specialistEnvVerbosity :: !Verbosity
      -- , specialistEnvInputSpecsFile :: !FilePath
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
      -- , specialistStateInputSpecs :: !(Map Text (DumpSpecInfo Text Text Text))
      , specialistStateOverloadedCallCount :: !Integer
      , specialistStateIORefBinds :: [CoreBind]
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
