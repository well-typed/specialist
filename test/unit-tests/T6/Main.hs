{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -ddump-simpl #-}

module Main where

import Class

instance MySuperClass a => MyClass a where
  {-# NOINLINE myMethod #-}
  myMethod x y = doConcat x y

instance MySuperClass Integer where
  {-# NOINLINE mySuperMethod #-}
  mySuperMethod = show

doConcat :: MySuperClass a => a -> a -> String
doConcat x y = mySuperMethod x ++ mySuperMethod y

main :: IO ()
main = do
  n <- readFile "Main.hs"
  putStrLn $ (if length n > 5 then konst (myMethod @Integer) else konst (\x y -> show (x + y))) () 5 10

{-# NOINLINE konst #-}
konst = const