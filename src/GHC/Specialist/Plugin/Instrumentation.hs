{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Types

import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.InfoProv
import GHC.IO (unsafePerformIO)
import Unsafe.Coerce

-- | Put a dictionary in a box
{-# NOINLINE dictToBox #-}
dictToBox :: forall c. Dict c -> Box
dictToBox = unsafeCoerce

-- | Put a dictionary for a constrain in a box
mkBox :: forall a. a => Box
mkBox = dictToBox (Dict @a)

-- | Traverse free variables of a dictionary to determine the superclasses
getDictInfo :: forall a. a -> IO DictInfo
getDictInfo d =
    DictInfo <$> whereFrom d <*> do
      getClosureData d >>=
        \case
          ConstrClosure _ ptrs _ _ _ _ -> do
            Just <$> mapM (\(Box fd) -> getDictInfo fd) ptrs
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
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
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
