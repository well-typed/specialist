module F3 where

{-# NOINLINE f3 #-}
f3 :: Eq a => a -> [a]
f3 x = if x == x then [x] else []
