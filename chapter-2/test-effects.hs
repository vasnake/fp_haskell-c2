-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Using foldr on tuple" #-}
{-# HLINT ignore "Using maximum on tuple" #-}
{-# HLINT ignore "Use print" #-}

module TestEffects where

import Text.Parsec (getParserState)
import Data.Char (toLower)

import Data.Monoid (
    Sum(..), Product (..), Endo(..), appEndo, (<>), Dual(..), First(..)
    )

import Control.Applicative (
    Applicative(..), (<*>), (<$>), ZipList(..)
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse
    )

-- import Control.Monad.Identity ( Identity(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor, fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe (..), null, ($), succ, (.), undefined, Num ((+)), Show, Eq,
    foldr, foldl, Either (..), Monoid (..), Semigroup (..), putStrLn, print, (*), (>), (/), (^),
    map
    )

test = foldr (+) 0 [0..42]
test2 = foldr (*) 3 (Just 14)
test3 = foldr (*) 3 (Right 14)
test4 = foldr (*) 3 (13, 14)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = foldr f ini r -- pre-order
    -- foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

test5 = fold [[1,2,3],[4,5]]
test6 = foldMap Sum [1,2,3,4]
test7 = foldMap Product [1,2,3,4]
test8 = maximum (99, 42)
-- test9 = appEndo (Endo (+2) `mappend` Endo (+3) `mappend` Endo(+4)) 1
test9 = appEndo (Endo (+2) <> Endo (+3) <> Endo (+4)) 1
test10 = "Abc" <> "De" <> "Fgh"
test11 = Dual "Abc" <> Dual "De" <> Dual "Fgh"
test12 = foldMap First [Nothing, Just 3, Just 5, Nothing]

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

test13 = sequenceA_ $ fmap (putStrLn . show) testTree
-- или еще проще
test14 = sequenceA_ $ fmap print testTree
test15 = traverse_ (\ x -> (show x, x*x)) [1, 2] -- ("12",())

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
-- sequenceA2list = foldr (\ x y -> pure (:) <*> x <*> y) (pure [])
sequenceA2list = foldr (\ x y -> ((:) <$> x) <*> y) (pure [])

-- test16 = sequenceA (map ZipList [[11,21], [12,22], [13,23]])
test16 = traverse ZipList [[11,21], [12,22], [13,23]]
