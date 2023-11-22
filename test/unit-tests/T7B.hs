{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module T7B where

-- A function with an overloaded call in its body. We do not want the
-- instrumentation to end up in the interface file, as if it got inlined later
-- we would probably instrument it again and cause potentially exponential blow
-- up in code size.
{-# INLINEABLE f #-}
f :: Show a => a -> Int
f x = length (show x)
