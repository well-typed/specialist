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

-- | The function that is injected by the Specialist plugin to instrument
-- overloaded function applications.
{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a r (b :: TYPE r).
     Addr#
  -- ^ Address of a String which is a unique identifier for this application
  -> Addr#
  -- ^ Address of a String which is the label of the most recent source note by
  -- this application
  -> Addr#
  -- ^ Address of a String which is the serialised RealSrcSpan from the most
  -- recent source note by this application
  -> (a -> b)
  -- ^ The overloaded function
  -> [IO (Maybe InfoProv)]
  -- ^ Computation that will get us the IPE info for the dictionaries used in
  -- the application
  -> ()
specialistWrapper' fIdAddr lAddr ssAddr f mdIpes = unsafePerformIO $ do
    -- This should result in the source location of the overloaded function
    fIpe <- whereFrom f

    -- This should result in the source location of the instance definitions
    -- being used in the overloaded call
    dIpes <- sequence mdIpes

    -- Unpack the information strings
    let
      fId = unpackCString# fIdAddr
      ss = unpackCString# ssAddr
      l = unpackCString# lAddr

    traceEventIO . show $
      SpecialistNote
        { specialistNoteId = fId
        , specialistNoteLocationLabel = l
        , specialistNoteLocationSpan = ss
        , specialistNoteFunctionIpe = fIpe
        , specialistNoteInstanceIpe = dIpes
        }

{-# NOINLINE specialistWrapper #-}
specialistWrapper :: forall a r (b :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => b)
  -> [IO (Maybe InfoProv)]
  -> ()
specialistWrapper = unsafeCoerce specialistWrapper'

-- {-# NOINLINE whereFrom' #-}
-- whereFrom' :: forall a. a ->  IO (Maybe InfoProv)
-- whereFrom' x = do
--     !ipePtr <- getIPE x
--     if ipePtr == nullPtr then
--       return Nothing
--     else do
--       return Nothing
--       -- infoProv <- peekInfoProv (ipeProv ipePtr)
--       -- return $ Just infoProv

{-# NOINLINE whereFromWrapper #-}
whereFromWrapper :: forall c. c => IO (Maybe InfoProv)
whereFromWrapper = unsafeCoerce whereFrom

{-# NOINLINE getDictIpeTypeDUMMY #-}
getDictIpeTypeDUMMY :: IO (Maybe InfoProv)
getDictIpeTypeDUMMY = error "I'm just here to be a Type, do not evaluate"
