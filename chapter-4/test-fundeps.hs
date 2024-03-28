{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE InstanceSigs #-}
-- module TestFundeps where
class Functor' c e | c -> e where fmap' :: (e -> e) -> c -> c
instance (Eq a)=> Functor' [a] a where fmap' = fmap
instance (Num a, Eq a)=> Functor' (Maybe a) a where fmap' = fmap

{--
class Mult a b c | a b -> c where
  (***) :: a -> b -> c

instance Mult Int Double Double where
  i *** d = fromIntegral i * d

--}
-- end of solution

-- test27 :: String
test27 = fmap' succ "ABC" -- "BCD"

-- test26 :: Maybe Integer
test26 = fmap' (^2) (Just 42) -- Just 1764
test28 = take 5 (fmap' (^ 2) [2, 3 .. ])
