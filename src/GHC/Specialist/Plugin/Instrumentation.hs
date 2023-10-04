{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Plugin.Types

import Control.Monad
import Data.Maybe
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.InfoProv
import GHC.IO (unsafePerformIO)
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
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> b)
  -> [Box]
  -> ()
specialistWrapper' fIdAddr lAddr ssAddr f boxedDicts =
    unsafePerformIO $ do
      -- Only emit about 1/100th of the times this path is executed
      coin <- randomRIO @Int (1, 100)
      when (coin == 42) $
        traceEventIO . show =<<
          SpecialistNote (unpackCString# fIdAddr)
            <$> currentCallStack
            <*> mapM (\(Box d) -> getDictInfo d) boxedDicts
            <*> whereFrom f
            <*> pure (unpackCString# lAddr)
            <*> pure (unpackCString# ssAddr)

specialistWrapper :: forall a r (b :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => b)
  -- ^ The overloaded function
  -> [Box]
  -- ^ 'Box'es holding the dictionaries used in the overloaded call
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'

-- | Just here to call @exprType@ on
boxTypeDUMMY :: Box
boxTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
