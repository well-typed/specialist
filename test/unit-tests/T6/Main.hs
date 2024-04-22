{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dumpdir=dumps #-}

module Main where

import Control.Applicative
import Class

{-# NOINLINE doConcat #-}
doConcat :: C1 a => a -> a -> String
doConcat x y = c1m2 x ++ c1m2 y

{-# NOINLINE doConcat1 #-}
doConcat1 :: C3 a => a -> a -> String
doConcat1 x y = doConcat x y

main :: IO ()
main = do
  n <- readFile "Main.hs" <|> pure "Well-Typed: The Haskell Consultants"
  putStrLn $ if length n > 5 then doConcat1 (Y 555) (Z 1.0) else "hey"
