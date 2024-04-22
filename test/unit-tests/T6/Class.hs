module Class where

class C1 a where
  c1m1 :: a -> Integer
  c1m2 :: a -> String

class C2 a where
  c2m1 :: a -> Bool
  c2m2 :: a -> Int

class (C1 a, C2 a) => C3 a where
  c3m1 :: String -> a -> Bool
  c3m2 :: (a, a, a) -> Int

data XYZ = X Bool | Y Int | Z Double
  deriving Show

instance C1 XYZ where
  c1m1 (X False) = 999
  c1m1 (X _)     = 111
  c1m1 (Y n)     = fromIntegral n
  c1m1 (Z d)     = if d > 100 then 1 else 500

  c1m2 = show

instance C2 XYZ where
  c2m1 (X False) = True
  c2m1 (X True)  = False
  c2m1 (Y n)     = n < 981
  c2m1 (Z d)     = d > 123

  c2m2 (X _)     = 321
  c2m2 (Y n)     = n
  c2m2 (Z d)     = fromEnum $ d > 123

instance C3 XYZ where
  c3m1 _       (X True) = False
  c3m1 "hello" _        = True
  c3m1 _       (X b)    = b
  c3m1 _       _        = False

  c3m2 (v1, v2, v3) = fromEnum $ c3m1 "hello" v1 && c3m1 "" v2 || c3m1 "finley" v3