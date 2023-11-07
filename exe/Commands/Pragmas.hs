module Commands.Pragmas where

import Commands.Common
import GHC.Specialist
import Utils

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.InfoProv
import Options.Applicative

data PragmasOptions =
    PragmasOptions
      { pragmasOptionsGetNotes :: IO (Either String [SpecialistNote])
      }

-- | Parse the options for the @pragmas@ command.
pragmasOptions :: Parser PragmasOptions
pragmasOptions =
        PragmasOptions
    <$> notesInput
    <**> helper

interpretPragmasCommand :: PragmasOptions -> IO ()
interpretPragmasCommand PragmasOptions{..} =
    pragmasOptionsGetNotes >>=
      \case
        Right notes ->
          prettyPrint $ foldl' go Map.empty notes
        Left msg ->
          perish $ "failed to get specialist notes from the input: " <> msg
  where
    go
      :: Map (InfoProv, [Maybe DictInfo]) Integer
      -> SpecialistNote
      -> Map (InfoProv, [Maybe DictInfo]) Integer
    go acc SpecialistNote{..} =
      case specialistNoteFunctionIpe of
        Just fIpe ->
          Map.insertWith (+) (fIpe, specialistNoteDictInfos) 1 acc
        Nothing ->
          acc

    prettyPrint
      :: Map (InfoProv, [Maybe DictInfo]) Integer
      -> IO ()
    prettyPrint result = do
        mapM_ printOne
      . reverse
      . sortOn snd
      $ Map.toList result

    printOne :: ((InfoProv, [Maybe DictInfo]), Integer) -> IO ()
    printOne ((fIpe, dictInfos), count) = do
      putStrLn "overloaded function with IPE information:"
      putStrLn $ "    " ++ show fIpe
      putStrLn $ "was called " ++ show count ++ " times with dictionaries " ++
                 "having the following IPE information:"
      mapM_ (putStrLn . ("    " ++) . show) dictInfos
      putStrLn ""

