{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE MagicHash           #-}

module GHC.Specialist.Plugin.Instrumentation where

import GHC.Specialist.Types

import Debug.Trace
import GHC.Exts
import GHC.InfoProv
import GHC.IO (unsafePerformIO)
import Unsafe.Coerce

{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a r (b :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> b)
  -> [IO (Maybe InfoProv)]
  -> ()
specialistWrapper' fIdAddr lAddr ssAddr f ds =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence ds
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper :: forall a r (b :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => b)
  -> [IO (Maybe InfoProv)]
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'


{-# NOINLINE whereFromWrapper #-}
whereFromWrapper :: forall c. c => IO (Maybe InfoProv)
whereFromWrapper = unsafeCoerce whereFrom

{-# NOINLINE getDictIpeTypeDUMMY #-}
getDictIpeTypeDUMMY :: IO (Maybe InfoProv)
getDictIpeTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
