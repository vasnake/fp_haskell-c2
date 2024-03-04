-- {-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Using foldr on tuple" #-}
{-# HLINT ignore "Using maximum on tuple" #-}
{-# HLINT ignore "Use print" #-}

module TestEffects where

import Text.Parsec (getParserState)
import Data.Char (toLower)
import Data.Function ( (&) )

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..)
    )

-- import Control.Monad.Identity ( Identity(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor ((<&>))

import Control.Applicative (
    Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>)
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

import Control.Monad ( liftM, mplus, guard, mfilter )

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take
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

newtype Const c a = Const { getConst :: c } deriving (Eq, Show)
-- фантомный тип (a), игнорирует второй параметр конструктора типа

instance Functor (Const m) where
    -- fmap :: (a -> b) -> Const c a -> Const c b
    fmap _ (Const v) = Const v

instance Foldable (Const c) where
    -- foldMap :: (Monoid m) => (a -> m) -> Const c a -> m
    foldMap _ _ = mempty -- поскольку в "контейнере" нет значений `a` (только "лог"), то это поведение естественно

instance (Monoid c) => Applicative (Const c) where
    -- pure :: a -> Const c a
    pure _ = Const mempty
    -- <*> :: Const c (a -> b) -> Const c a -> Const c b -- a, b это фантомы, их нет
    (<*>) (Const f) (Const v) = Const (mappend f v) -- семантика пары: (лог, _)

instance Traversable (Const c) where
    -- traverse :: (Applicative f) => (a -> f b) -> Const c a -> f (Const c b)
    traverse _ (Const v) = pure (Const v) -- подняли в аппликатив и поменяли метку типа с `a` на `b`

test17 = Const 'z'

data Result a = Error | Ok a deriving (Eq, Show) -- sum-type, Maybe isomorph
instance Functor Result where fmap = fmapDefault -- используя траверс
instance Foldable Result where foldMap = foldMapDefault -- используя траверс

instance Traversable Result where
-- instance (Functor Result, Foldable Result) => Traversable Result where
    -- traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
    traverse _ Error = pure Error
    -- traverse f (Ok x) = (pure Ok) <*> (f x)
    traverse f (Ok x) = Ok <$> f x

test18 = traverse (\x -> [x+10, x+20]) (Ok 5)

(<***>) :: Applicative f => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)

funM mv = mv >>= (\v -> if v > 0 then return (v ^ 2) else fail $ "got negative value: " ++ show v)

funA :: (Applicative f, Num a, Ord a) => f a -> f a
-- funA mv = pure (\x -> if x > 0 then x^2 else undefined) <*> mv
funA mv = (\x -> if x > 0 then x^2 else undefined) <$> mv

test19 = Nothing <|> Just 3 <|> Just 5 <|> Nothing
test20 = Nothing `mplus` Just 3 `mplus` Just 5 `mplus` Nothing

pythags = do 
    z <- [1 ..]
    x <- [1 .. z]
    y <- [x .. z]
    guard (x^2 + y^2 == z^2)
    return (x, y, z)

pythagsL = [(x,y,z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]
test21 = msum [Nothing, Just 3, Just 5, Nothing]
test22 = mfilter (>3) (Just 4)
