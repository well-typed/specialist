{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}

-- Running this program should result in two calls to overloaded functions: One
-- with the $fShowX dictionary, the next with the $fShowList dictionary
-- constructor for X.

module Main where

-- Causes the application of $fShowList to the given dictionary. The application
-- of `f` here is obviously overloaded and should trigger output from the
-- plugin. Also note that although the `$fShowList` dictionary constructor is
-- overloaded, it should not trigger any output from the plugin when it is
-- applied to the dictionary since we avoid detecting overloaded calls that
-- result in dictionaries.
{-# NOINLINE invoke #-}
invoke :: Show a => (Show [a] => [a] -> String) -> a -> String
invoke f x = f [x]

data X = X
    deriving Show

-- The application of `invoke` here is overloaded, as it should be given the
-- `$fShowX` dictionary, which will have `$fShowList` selected out of it in the
-- body of `invoke`.
main :: IO ()
main = putStrLn (invoke show X)
