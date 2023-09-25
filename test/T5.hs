module Main where

data X = X1 | X2
  deriving (Show, Read, Eq, Ord)

{-# NOINLINE doSomething #-}
doSomething :: Ord a => a -> a -> Bool
doSomething x1 x2 = x2 < x1

main :: IO ()
main = do
    print $ doSomething X1 X2 -- This application should show only the Eq constraint as a FV
