{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Plugin.Types

import Control.Concurrent
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.InfoProv
import GHC.IO
import System.Random
import Unsafe.Coerce

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
-- functions without adding the functions themselves as superclasses.
--
-- Additionally, we would like to avoid adding the same superclass twice if it
-- is referenced by two class functions. I.e., we only want to add superclasses
-- encountered on distinct paths through other superclasses, not distinct paths
-- through class functions. Therefore we accumulate the superclasses we have
-- encountered as direct references from a closure in a set.
getDictInfo :: forall a. Set DictInfo -> a -> IO (Set DictInfo)
getDictInfo = go
  where
    go :: forall d. Set DictInfo -> d -> IO (Set DictInfo)
    go acc d =
      getClosureData d >>=
        \case
          ConstrClosure _ ptrs _ _ _ dcon_nm | 'C':':':_ <- dcon_nm -> do
            wf <- whereFrom d

            -- We reset the accumulator to the empty set here, since we no
            -- longer wish to avoid adding duplicate superclasses (we may
            -- encounter the same dictionary along a different path of
            -- superclasses)
            frees <- foldM (\scs (Box fd) -> go scs fd) Set.empty ptrs

            return (Set.singleton $ DictInfo wf (Set.toList frees))
          FunClosure _ ptrs _ ->
            -- Use the same accumulator here, we do not consider references
            -- through class functions as distinct
            foldM (\scs (Box fd) -> go scs fd) acc ptrs
          _ ->
            return Set.empty

{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a r (b :: TYPE r).
     Double
  -> Addr#
  -> Addr#
  -> Addr#
  -> (a -> b)
  -> [Box]
  -> ()
specialistWrapper' sampleProb fIdAddr lAddr ssAddr f boxedDicts =
    unsafePerformIO $ do
      coin <- (< sampleProb) <$> randomRIO @Double (0.0, 1.0)
      when coin $
        traceEventIO . show =<<
          SpecialistNote (unpackCString# fIdAddr)
            <$> currentCallStack
            <*> (reverse <$> currentCallStackIds)
            <*> (Set.toList <$> foldM (\acc (Box d) -> getDictInfo acc d) Set.empty boxedDicts)
            <*> whereFrom f
            <*> pure (unpackCString# lAddr)
            <*> pure (unpackCString# ssAddr)
            <*> fmap (fromIntegral . fst) (myThreadId >>= threadCapability)

specialistWrapper :: forall a r (b :: TYPE r).
     Double
  -- ^ Sample probability
  -> Addr#
  -- ^ Unique identifier for this overloaded call site
  -> Addr#
  -- ^ Label of the last source tick we encountered
  -> Addr#
  -- ^ Source span of the last source tick we encountered
  -> (a => b)
  -- ^ The overloaded function
  -> [Box]
  -- ^ 'Box'es holding the dictionaries used in the overloaded call
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'

-- | Just here to call @exprType@ on
boxTypeDUMMY :: Box
boxTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
