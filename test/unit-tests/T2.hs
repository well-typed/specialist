module Main where

-- `print` is overloaded here, should get `$fShowList @Char $fShowChar`
main = print calls

data X = X

instance Show X where
  show X = "X"

instance Semigroup X where
  X <> X = X

data Box a = Box a


instance Show a => Show (Box a) where
  show (Box x) = show x

{-# NOINLINE overloaded #-}
overloaded :: (Show a) => a -> String
overloaded x = show x

calls :: String
calls =
    overloaded (Just X)

    -- Does not trigger?
    -- ((\_ -> "hello") :: Show a => a -> String ) X


