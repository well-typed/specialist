{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImpredicativeTypes #-}

module GHC.Specialist.Wrapper where

import GHC.Specialist.Types

import GHC.Exts
import GHC.InfoProv
import GHC.IO (unsafePerformIO)
import Unsafe.Coerce

-- | The function that is injected by the Specialist plugin to wrap all
-- overloaded function applications.
specialistWrapper'
  :: Addr#
  -- ^ Address of some basic information about the application
  -> (a -> b)
  -- ^ The overloaded function
  -> a
  -- ^ The dictionary used for the overloaded call
  -> b
specialistWrapper' metaAddr f d = unsafePerformIO $ do
    -- This should result in the source location of the instance definition
    -- being used for the overloaded function
    dIpe <- whereFrom d

    -- This should be the definition site of the function being applied
    fIpe <- whereFrom f

    -- Unpack the meta information string
    let meta = unpackCString# metaAddr

    appendFile outFile . (++ "\n") . show $
      SpecialistNote
        { specialistNoteFunctionInfoProv = fIpe
        , specialistNoteInstanceInfoProv = dIpe
        , specialistNoteMeta = meta
        }

    return (f d)
  where
    outFile = "specialist-notes.txt"

specialistWrapper :: Addr# -> (a => b) -> (a => b)
specialistWrapper = unsafeCoerce specialistWrapper'