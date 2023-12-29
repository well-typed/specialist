module Main where

-- At one point, the plugin would cause core lint errors on this code due to the
-- constraint selector generated for the X a instance. The core looks like this:
--
-- Main.$fXa_$cp1X
--   = \ @a ($d~ :: a ~ Int) ->
--       case eq_sel @(*) @a @Int $d~ of co
--       { __DEFAULT ->
--       GHC.Num.$fNumInt
--       `cast` ((Num (Sym co))_R :: Num Int ~R# Num a)
--       }
--
-- And that eq_sel would get instrumented, causing core lint errors. The plugin
-- now has a check that ensures we do not instrument calls whose results are
-- "constraint like", so we should test that this program properly compiles with
-- the plugin and core lint enabled.

class    Num a   => X a where
instance a ~ Int => X a where

main :: IO ()
main = putStrLn "should compile with plugin"
