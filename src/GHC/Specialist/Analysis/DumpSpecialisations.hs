module GHC.Specialist.Analysis.DumpSpecialisations where

import Control.Monad
import Data.Maybe
import GHC.Types.DumpSpecInfo
import Text.Read

readDumpSpecInfosFromFile
  :: Read (DumpSpecInfo a b c)
  => FilePath
  -> IO [DumpSpecInfo a b c]
readDumpSpecInfosFromFile =
    pure . mapMaybe readMaybe . lines <=< readFile
