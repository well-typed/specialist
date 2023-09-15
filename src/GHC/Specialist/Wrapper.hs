{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash          #-}

module GHC.Specialist.Wrapper where

import GHC.Specialist.Types

import GHC.Exts
import GHC.InfoProv
import GHC.IO (unsafePerformIO)
import Unsafe.Coerce

-- | The function that is injected by the Specialist plugin to instrument
-- overloaded function applications.
{-# NOINLINE specialistWrapper' #-}
specialistWrapper' :: forall a.
     Addr#
  -- ^ Address of a String which is a unique identifier for this application
  -> Addr#
  -- ^ Address of the String which is the serialised RealSrcSpan from the most
  -- recent source note by this application
  -> Addr#
  -- ^ Address of some serialised metadata about the application
  -> a
  -- ^ The dictionary used for the overloaded call
  -> ()
specialistWrapper' fIdAddr ssAddr metaAddr d = unsafePerformIO $ do
    -- This should result in the source location of the instance definition
    -- being used for the overloaded function
    dIpe <- whereFrom d

    -- Unpack the meta information string
    let
      fId = unpackCString# fIdAddr
      ss = unpackCString# ssAddr
      meta = unpackCString# metaAddr

    appendFile outFile . (++ "\n") . show $
      SpecialistNote
        { specialistNoteFunctionId = fId
        , specialistNoteSourceSpan = ss
        , specialistNoteMeta = meta
        , specialistNoteInstanceInfoProv = dIpe
        }

    return ()
  where
    outFile = "specialist-notes.txt"

{-# NOINLINE specialistWrapper #-}
specialistWrapper :: forall a.
     Addr#
  -> Addr#
  -> Addr#
  -> (a => ())
specialistWrapper = unsafeCoerce specialistWrapper'
