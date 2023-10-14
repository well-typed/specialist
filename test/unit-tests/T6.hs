module T6 where

{-# SPECIALISE specMe :: Int -> Int -> Int -> Bool #-}
specMe :: forall a. Eq a => a -> a -> a -> Bool
specMe x y z = x /= y && x /= z && y /= z

specMeInt :: Int -> Int -> Int -> Bool
specMeInt = specMe @Int


