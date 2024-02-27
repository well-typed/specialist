module F2 where

import F3
import Data.Maybe

{-# NOINLINE f2 #-}
f2 :: Ord a => a -> Maybe a
f2 = listToMaybe . f3

