{-# OPTIONS_GHC -ddump-verbose-inlinings #-}

module T6 where


-- Causes both specialisations to be generated
{-# INLINE specMe #-}

-- Causes no specialisations to be generated
-- {-# INLINABLE specMe #-}

-- Causes exactly the specialisation you'd expect
-- {-# SPECIALISE specMe :: Int -> Int -> Int -> Bool #-}
specMe :: forall a. Eq a => a -> a -> a -> Bool
specMe x y z = x /= y && x /= z && y /= z

specMeInt :: Int -> Int -> Int -> Bool
specMeInt = specMe

specMeBool :: Bool -> Bool -> Bool -> Bool
specMeBool = specMe


