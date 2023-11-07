{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Plugin.Types

import Control.Concurrent
import Control.Monad
import Data.Maybe
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

-- | Put a dictionary for a constrain in a box
mkBox :: forall a. a => Box
mkBox = dictToBox (Dict @a)

-- | Traverse free variables of a dictionary to determine the superclasses
getDictInfo :: forall a. a -> IO (Maybe DictInfo)
getDictInfo d = do
    getClosureData d >>=
      \case
        ConstrClosure _ ptrs _ _ _ dcon_nm
          | 'C':':':_ <- dcon_nm -> do
            wf <- whereFrom d
            frees <- catMaybes <$> mapM (\(Box fd) -> getDictInfo fd) ptrs
            return $ Just $ DictInfo wf frees
        _ ->
          return Nothing

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
            <*> mapM (\(Box d) -> getDictInfo d) boxedDicts
            <*> whereFrom f
            <*> pure (unpackCString# lAddr)
            <*> pure (unpackCString# ssAddr)
            <*> (fmap (fromIntegral . fst) $ myThreadId >>= threadCapability)

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
