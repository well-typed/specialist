{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Main where

-- Forces the application of $fShowList to the given dictionary. The application
-- of `f` here is obviously overloaded and should trigger output from the
-- plugin. Also note that although that `$fShowList` function is overloaded, it
-- should not trigger any output from the plugin when it is applied to the
-- dictionary since we avoid detecting overloaded calls that result in
-- dictionaries.
invoke :: Show a => (Show [a] => [a] -> String) -> a -> String
invoke f x = f [x]
{-# NOINLINE invoke #-}

data X = X
    deriving Show

-- The application of `invoke` here is overloaded, as it should be given the
-- `$fShowX` dictionary, which will have `$fShowList` selected out of it in the
-- body of `invoke`.
main = putStrLn (invoke show X)
