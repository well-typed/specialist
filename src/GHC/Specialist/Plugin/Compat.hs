{-# LANGUAGE CPP #-}

module GHC.Specialist.Plugin.Compat where

#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
import GHC.Data.FastString
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
sourceNoteLabelString :: LexicalFastString -> String
sourceNoteLabelString (LexicalFastString fs) = unpackFS fs
#else
sourceNoteLabelString :: String -> String
sourceNoteLabelString = id
#endif
