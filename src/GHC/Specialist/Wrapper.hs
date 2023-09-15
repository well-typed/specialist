{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash          #-}

module GHC.Specialist.Wrapper where

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
  -> a
  -- ^ The dictionary used for the overloaded call
  -> ()
specialistWrapper' fIdAddr lAddr ssAddr f d = unsafePerformIO $ do
    -- This should result in the source location of the overloaded function
    fIpe <- whereFrom f

    -- This should result in the source location of the instance definition
    -- being used for the overloaded function
    dIpe <- whereFrom d

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
        , specialistNoteInstanceIpe = dIpe
        }

{-# NOINLINE specialistWrapper #-}
specialistWrapper :: forall a r (b :: TYPE r).
     Addr#
  -> Addr#
  -> Addr#
  -> (a => b)
  -> (a => ())
specialistWrapper = unsafeCoerce specialistWrapper'
