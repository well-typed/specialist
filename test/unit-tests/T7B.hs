module T7B where

-- A function with an overloaded call in its body
{-# INLINEABLE f #-}
f :: Show a => a -> Int
f x = length (show x)
