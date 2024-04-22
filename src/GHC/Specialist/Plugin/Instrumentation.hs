{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Plugin.Types

import Control.Concurrent
import Control.Monad
import Data.List
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.InfoProv
import GHC.IORef
import GHC.IO
import System.Random
import Unsafe.Coerce
import Data.Maybe

-- | Put a dictionary in a box
{-# NOINLINE dictToBox #-}
dictToBox :: forall c. Dict c -> Box
dictToBox = unsafeCoerce

-- | Put a dictionary as a constraint in a box
mkBox :: forall a. a => Box
mkBox = dictToBox (Dict @a)

-- | Traverse free variables of a dictionary to determine the superclasses. For
-- some reason, the references to superclass dictionaries (sometimes?) go
-- through the class functions, so we need to follow references through the
-- functions and thunks without adding the functions or thunks themselves as
-- superclasses.
--
-- Additionally, we would like to avoid adding the same superclass twice if it
-- is referenced by two class functions. I.e., we only want to add superclasses
-- encountered on distinct paths through other superclasses, not distinct paths
-- through class functions. Therefore we accumulate the superclasses we have
-- encountered as direct references from a closure in a set.
getDictInfo :: (Box, String) -> IO DictInfo
getDictInfo (box@(Box dict), prettyType) = do
    dc <-
      getClosureData dict >>=
        \case
          ConstrClosure _ ptrs _ _ _ dcon_nm | 'C':':':cls_nm <- dcon_nm -> do
            wf <- whereFrom dict

            let
              -- If the class name of this closure is found in ptr closures,
              -- assume it is actually a class function, not a superclass
              --
              -- cls_nm will look like "C:Eq_Main_0_con_info" and we want to
              -- filter out references to things like "$fEqX_$c==_info", so we
              -- look for the "Eq"
              filt InfoProv{..} = not $ takeWhile (/= '_') cls_nm `isInfixOf` ipName

            frees <- catMaybes <$> mapM (go filt) ptrs
            return $ DictClosure wf frees
          FunClosure _ ptrs _ -> do
            -- Assume this is a single method dictionary, which is actually just
            -- the function
            wf <- whereFrom dict
            frees <- catMaybes <$> mapM (go (const True)) ptrs
            return $ DictClosure wf frees
          ThunkClosure _ ptrs _ -> do
            -- TODO: For some reason, some dictionaries are showing up as
            -- thunks. If we force them, I think some will show up as PAPs?
            wf <- whereFrom dict
            frees <- catMaybes <$> mapM (go (const True)) ptrs
            return $ DictClosure wf frees
          closure ->
            go (const True) box >>=
              \case
                Just dc ->
                  return dc
                Nothing -> do
                  wf <- whereFrom dict
                  return (DictClosureRaw wf (show closure))

    return $ DictInfo prettyType dc
  where
    go :: (InfoProv -> Bool) -> Box -> IO (Maybe DictClosure)
    go ipeFilt (Box d) =
      getClosureData d >>=
        \case
          ConstrClosure _ ptrs _ _ _ dcon_nm | 'C':':':cls_nm <- dcon_nm -> do
            wf <- whereFrom d

            let
              -- If the class name of this closure is found in ptr closures,
              -- assume it is actually a class function, not a superclass
              --
              -- cls_nm will look like "C:Eq_Main_0_con_info" and we want to
              -- filter out references to things like "$fEqX_$c==_info", so we
              -- look for the "Eq"
              filt InfoProv{..} = not $ takeWhile (/= '_') cls_nm `isInfixOf` ipName

            frees <- catMaybes <$> mapM (go filt) ptrs
            return $ Just (DictClosure wf frees)
          FunClosure _ ptrs _ -> do
            -- Assume this is a single method dictionary, which is actually just
            -- the function
            --
            -- If the filter is "False", then we assume this is a function
            -- referenced by a record dictionary, so we don't want to keep it
            wf <- whereFrom d
            if maybe True ipeFilt wf then do
              frees <- catMaybes <$> mapM (go (const True)) ptrs
              return $ Just (DictClosure wf frees)
            else do
              return Nothing
          IndClosure _ ptr -> do
            -- Go straight through indirections
            --
            -- I believe this can happen if, e.g., a dictionary is given a cost
            -- center with -fprof-late.
            go ipeFilt ptr
          PAPClosure _ _ _ _ptrs _ -> do
            -- TODO: For some reason, some dictionaries were showing up as
            -- PAPs... no idea why. This was happening before I added the thunk
            -- closures back and stopped forcing the dict to WHNF, maybe they
            -- were resulting from forced dicts?
            return Nothing
          BlackholeClosure _ ind -> do
            -- Blackholes can be updated to point to indirections, if things are
            -- timed just right. See Note [BLACKHOLE pointing to IND] in GHC.
            ind_cd <- getClosureData ind
            case ind_cd of
              IndClosure _ ptr ->
                go ipeFilt ptr
              _ ->
                -- TODO: When the plugin is enabled on the Cabal-syntax library,
                -- and we run the cabal hackage-test parsec test, we see many
                -- dictionaries come through as BlackholeClosures. It's unclear
                -- what exactly is resulting in this, but as a first-try
                -- workaround I added this retry loop (instead of `return
                -- Nothing`) and it fixed it. Definitely worth keeping in mind,
                -- in case this ends up biting us later.
                let !d' = d in go ipeFilt (Box d')
                -- return Nothing
          _c -> do
            return Nothing

{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a r (b :: TYPE r).
     Double
  -> IORef Bool
  -> Addr#
  -> Addr#
  -> Addr#
  -> (a -> b)
  -> [(Box, String)]
  -> ()
specialistWrapper' sampleProb hasSampledRef fIdAddr lAddr ssAddr f boxedDicts =
    unsafePerformIO $ do
      coin <- (< sampleProb) <$> randomRIO @Double (0.0, 1.0)
      sampled <- atomicModifyIORef' hasSampledRef (True,)
      when (coin || not sampled) $
        traceEventIO . show =<<
          SpecialistNote (unpackCString# fIdAddr)
            <$> currentCallStack
            <*> (reverse <$> currentCallStackIds)
            <*> mapM getDictInfo boxedDicts
            <*> whereFrom f
            <*> pure (unpackCString# lAddr)
            <*> pure (unpackCString# ssAddr)
            <*> fmap (fromIntegral . fst) (myThreadId >>= threadCapability)

specialistWrapper :: forall a r (b :: TYPE r).
     Double
  -- ^ Sample probability
  -> IORef Bool
  -- ^ 'IORef' holding a 'Bool' indicating whether a sample has been emitted for
  -- this overloaded call yet.
  -> Addr#
  -- ^ Unique identifier for this overloaded call site
  -> Addr#
  -- ^ Label of the last source tick we encountered
  -> Addr#
  -- ^ Source span of the last source tick we encountered
  -> (a => b)
  -- ^ The overloaded function
  -> [(Box, String)]
  -- ^ 'Box'es holding the dictionaries used in the overloaded call, paired with
  -- their pretty-printed types
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'

-- | Just here to call @exprType@ on
boxTypeDUMMY :: Box
boxTypeDUMMY = error "I'm just here to be a Type, do not evaluate"

-- | Just here to call @exprType@ on
iorefBoolTypeDUMMY :: IORef Bool
iorefBoolTypeDUMMY = error "I'm just here to be a Type, do not evaluate"

boolTypeDUMMY :: Bool
boolTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
