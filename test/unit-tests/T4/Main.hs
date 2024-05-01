module Main where

-- Here we make sure that superclasses are tracked properly when the superclass
-- relationship forms an acyclic graph rather than a tree. The relationship of
-- the superclasses of C2 is as follows:
--
--       C2
--      /  \
--    C1    Ord
--      \  /
--       Eq
--
-- So the Eq instance should show up as a free dictionary of both the Ord and C1
-- dictionaries.
--
-- We also have a single-method type class without superclasses to test the fact
-- that those are simply represented as the functions themselves, not records.
-- But TODO to actually add an automated check that it shows up properly.

data X = X1 | X2
  deriving (Show, Read, Eq, Ord)

class Eq a => C1 a where
  lt :: a -> a -> Bool
  dc1 :: a -> ()
  dc1 _ = ()

class (Ord a, C1 a) => C2 a where
  gt :: a -> a -> Bool
  dc2 :: a -> ()
  dc2 _ = ()

class C3 a where
  c3 :: a -> a -> a -> Bool

instance C1 X where
  {-# NOINLINE lt #-}
  lt = (<)

instance C2 X where
  {-# NOINLINE gt #-}
  gt x1 x2 = x1 `lt` x2 && x1 > x2

instance C3 X where
  {-# NOINLINE c3 #-}
  c3 x1 x2 x3 = x1 `lt` x2 || x1 > x2 && x2 > x3

{-# NOINLINE doSomething #-}
doSomething :: (C3 a, C2 a) => a -> a -> Bool
doSomething x y = gt x y || c3 x y x

main :: IO ()
main = do
    print $
      -- This application should show only the Eq constraint as a superclass
      X1 `doSomething` X2
