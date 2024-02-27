module F1 where

import F2

{-# NOINLINE f1 #-}
f1 :: Maybe Int
f1 = f2 1234
