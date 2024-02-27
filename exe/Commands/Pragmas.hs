module Commands.Pragmas where

import GHC.Specialist

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.InfoProv
import Options.Applicative

data PragmasOptions =
    PragmasOptions
      -- | Omit notes for functions whose IPE information indicates they are
      -- defined in modules containing these substrings
      [String]
  deriving (Show, Read, Eq)

-- | Parse the options for the @pragmas@ command.
pragmasOptions :: Parser PragmasOptions
pragmasOptions =
        PragmasOptions
    <$> many
          ( strOption
              (    long "omit-modules"
                <> metavar "SUBSTRING"
                <> help
                      ( "Omit overloaded functions originating from modules " ++
                        "whose names contain SUBSTRING"
                      )
              )
          )
    <**> helper

pragmas :: PragmasOptions -> [SpecialistNote] -> IO ()
pragmas (PragmasOptions omitModules) =
    pretty . foldl' go Map.empty
  where
    go
      :: Map (InfoProv, [DictInfo]) Integer
      -> SpecialistNote
      -> Map (InfoProv, [DictInfo]) Integer
    go acc SpecialistNote{..} =
        case specialistNoteFunctionIpe of
          Just fIpe ->
            if omitNote fIpe then
              acc
            else
              Map.insertWith (+) (fIpe, specialistNoteDictInfos) 1 acc
          Nothing ->
            acc
      where
        omitNote ipe = any (\m -> m `isInfixOf` ipMod ipe) omitModules


    pretty
      :: Map (InfoProv, [DictInfo]) Integer
      -> IO ()
    pretty result = do
          mapM_ printOne
        . reverse
        . sortOn snd
        $ Map.toList result

    printOne :: ((InfoProv, [DictInfo]), Integer) -> IO ()
    printOne ((ip, dictInfos), count) = do
        putStrLn $ concat
          [ ipMod ip, ".", ipLabel ip
          , " (", ipSrcFile ip, ":", ipSrcSpan ip, ")"
          ]
        putStrLn $
          "  called " ++ show count ++ " times with dictionaries: "
        mapM_
          ( \(DictInfo t c) -> do
              putStr $ "    " ++ t
              case dictClosureProv c of
                Just InfoProv{..} ->
                  putStrLn $
                    " (" ++ ipMod ++ "." ++ ipLabel ++ " " ++
                    ipSrcFile ++ ":" ++ ipSrcSpan ++ ")"
                Nothing ->
                  putStrLn ""
          )
          dictInfos
