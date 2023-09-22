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

{-# NOINLINE specialistWrapper1' #-}
specialistWrapper1' :: forall a r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> z)
  -> (a -> ())
specialistWrapper1' fIdAddr lAddr ssAddr f d =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence
                [ whereFrom d
                ]
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper1 :: forall a r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => z)
  -> (a => ())
specialistWrapper1 = unsafeCoerce specialistWrapper1'

{-# NOINLINE specialistWrapper2' #-}
specialistWrapper2' :: forall a b r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> z)
  -> (a -> b -> ())
specialistWrapper2' fIdAddr lAddr ssAddr f d1 d2 =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence
                [ whereFrom d1
                , whereFrom d2
                ]
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper2 :: forall a b r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => z)
  -> (a => b => ())
specialistWrapper2 = unsafeCoerce specialistWrapper2'

{-# NOINLINE specialistWrapper3' #-}
specialistWrapper3' :: forall a b c r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> z)
  -> (a -> b -> c -> ())
specialistWrapper3' fIdAddr lAddr ssAddr f d1 d2 d3 =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence
                [ whereFrom d1
                , whereFrom d2
                , whereFrom d3
                ]
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper3 :: forall a b c r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => z)
  -> (a => b => c => ())
specialistWrapper3 = unsafeCoerce specialistWrapper3'

{-# NOINLINE specialistWrapper4' #-}
specialistWrapper4' :: forall a b c d r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> z)
  -> (a -> b -> c -> d -> ())
specialistWrapper4' fIdAddr lAddr ssAddr f d1 d2 d3 d4 =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence
                [ whereFrom d1
                , whereFrom d2
                , whereFrom d3
                , whereFrom d4
                ]
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper4 :: forall a b c d r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => z)
  -> (a => b => c => d => ())
specialistWrapper4 = unsafeCoerce specialistWrapper4'

{-# NOINLINE specialistWrapper5' #-}
specialistWrapper5' :: forall a b c d e r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a -> z)
  -> (a -> b -> c -> d -> e -> ())
specialistWrapper5' fIdAddr lAddr ssAddr f d1 d2 d3 d4 d5 =
    unsafePerformIO $
      traceEventIO . show =<<
        SpecialistNote
          <$> pure (unpackCString# fIdAddr)
          <*> sequence
                [ whereFrom d1
                , whereFrom d2
                , whereFrom d3
                , whereFrom d4
                , whereFrom d5
                ]
          <*> whereFrom f
          <*> pure (unpackCString# lAddr)
          <*> pure (unpackCString# ssAddr)

specialistWrapper5 :: forall a b c d e r (z :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => z)
  -> (a => b => c => d => e => ())
specialistWrapper5 = unsafeCoerce specialistWrapper5'
