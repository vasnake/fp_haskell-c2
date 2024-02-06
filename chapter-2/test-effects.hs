-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}

module TestEffects where

import Text.Parsec (getParserState)
import Data.Char (toLower)
import Control.Applicative hiding (many)
import Data.Foldable (Foldable, fold, foldMap, maximum)

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor, fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe (..), null, ($), succ, (.), undefined, Num ((+)), Show, Eq,
    foldr, foldl, Either (..)
    )
import Data.Monoid (Sum(..), Product (..))

test = foldr (+) 0 [0..42]
test2 = foldr (*) 3 (Just 14)
test3 = foldr (*) 3 (Right 14)
test4 = foldr (*) 3 (13, 14)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5

instance Foldable Tree where
    foldr f ini Nil = ini
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = (foldr f ini r) -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

test5 = fold [[1,2,3],[4,5]]
test6 = foldMap Sum [1,2,3,4]
test7 = foldMap Product [1,2,3,4]
test8 = maximum (99, 42)
