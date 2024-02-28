-- https://pastebin.com/bUAyStqG
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Test_1_2_3 where
import Prelude hiding (Applicative, pure, (<*>))

infixl 4 <*>

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b  

instance Applicative [] where
    pure x = [x,x]
    fs <*> xs = [f x | f <- fs, x <- xs]

x = 3
g = (*2)
us = [(+7)]
vs = [(+9)]
fs = [\x -> 3*x, \x -> 4*x]
xs = [3,4]

comp1 = (.) <$> us <*> vs <*> xs
comp2 = us <*> (vs <*> xs)

iden1 = pure id <*> xs
iden2 = xs

appfun1 = g <$> xs
appfun2 = pure g <*> xs

homo1 = pure g <*> pure x :: [Integer]
homo2 = pure (g x) :: [Integer]

inter1 = fs <*> pure x
inter2 = pure ($ x) <*> fs

main = do
    putStrLn ("Composition:\t" ++ show comp1 ++ " ≡  " ++ show comp2)
    putStrLn ("Identity:\t" ++ show iden1 ++ " ≡  " ++ show iden2)
    putStrLn ("Appl-Functor:\t" ++ show appfun1 ++ " ≡  " ++ show appfun2)
    putStrLn ("Homomorphism:\t" ++ show homo1 ++ " ≡  " ++ show homo2)
    putStrLn ("Interchange:\t" ++ show inter1 ++ " ≡  " ++ show inter2)
{--
ghci> :load chapter-1/test-1.2.3.hs
ghci> main
Composition:    [19,20] ≡  [19,20]
Identity:       [3,4,3,4] ≡  [3,4]
Appl-Functor:   [6,8] ≡  [6,8,6,8]
Homomorphism:   [6,6,6,6] ≡  [6,6]
Interchange:    [9,9,12,12] ≡  [9,12,9,12]
--}
