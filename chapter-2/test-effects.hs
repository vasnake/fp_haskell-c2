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
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Fuse foldMap/fmap" #-}
{-# HLINT ignore "Fuse foldMap/map" #-}

module TestEffects where

import Text.Parsec (getParserState)
import Data.Char ( toLower, isDigit, )
import Data.Function ( (&), )

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..),
    getAny, Any(..), getLast, Last(..), getAll, All(..),
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
    show, read, String, Char, Functor(..), fmap, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take,
    even, Bool(..),
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
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = foldr f ini r -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order

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


{--
Сделайте тип

data Triple a = Tr a a a  deriving (Eq, Show)

представителем класса типов `Foldable`:

GHCi> foldr (++) "!!" (Tr "ab" "cd" "efg")
"abcdefg!!"
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
--}

-- class Foldable t where
--     foldr :: (a -> b -> b) -> b -> t a -> b
--     foldl :: (b -> a -> b) -> b -> t a -> b

data Triple a = Tr a a a  deriving (Eq, Show)
instance Foldable Triple where
    -- foldr :: (a -> b -> b) -> b -> Triple a -> b
    -- foldr f ini (Tr x y z) = f x (f y (f z ini))
    -- foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))

    -- foldMap :: (Monoid m) => (a -> m) -> Triple a -> m
    -- Map each element of the structure into a monoid, and combine the results with (<>).
    -- This fold is right-associative and lazy in the accumulator.
    foldMap f (Tr x y z) = (f x) `mappend` ((f y) `mappend` (f z))

test23 = foldr (++) "!!" (Tr "ab" "cd" "efg") -- "abcdefg!!"
test24 = foldl (++) "!!" (Tr "ab" "cd" "efg") -- "!!abcdefg"


{--
Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева
https://en.wikipedia.org/wiki/Tree_traversal

Сделайте двоичное дерево `Tree`

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

представителем класса типов `Foldable`
реализовав симметричную стратегию (in-order traversal)

Реализуйте также три другие стандартные стратегии 
(pre-order traversal, post-order traversal и level-order traversal), 
сделав типы-обертки представителями класса `Foldable`

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

GHCi> tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
GHCi> foldr (:) [] tree
[1,2,3,4]
GHCi> foldr (:) [] $ PreO tree
[3,1,2,4]
GHCi> foldr (:) [] $ PostO tree
[2,1,4,3]
GHCi> foldr (:) [] $ LevelO tree
[3,1,4,2]

In-order, LNR
Recursively traverse the current node's left subtree.
Visit the current node.
Recursively traverse the current node's right subtree

Pre-order, NLR
Visit the current node.
Recursively traverse the current node's left subtree.
Recursively traverse the current node's right subtree.

Post-order, LRN
Recursively traverse the current node's left subtree.
Recursively traverse the current node's right subtree.
Visit the current node.

flatTree :: Tree a -> [a]
flatTree Nil = [] 
flatTree (Branch l x r) = 
   flatTree l ++ [x] ++ flatTree r  -- In-order
   [x] ++ flatTree l ++ flatTree r  -- Pre-order
   flatTree l ++ flatTree r ++ [x]  -- Post-order

самое правое слагаемое идёт как ini в очередной foldr

           3
         /   \
        1     4
         \
          2
--}

-- data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
-- instance Foldable Tree where
--     foldr f ini Nil = ini
--     foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order, LNR

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
    foldr :: (a -> b -> b) -> b -> Preorder a -> b
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f iniR (PreO l)) where iniR = foldr f ini (PreO r) -- pre-order, NLR

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
    foldr :: (a -> b -> b) -> b -> Postorder a -> b
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f foldedRight (PostO l)  where -- post-order, LRN
        foldedRight = foldr f foldedNode (PostO r) -- RN
        foldedNode = f x ini

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
instance Foldable Levelorder where
    -- Map each element of the structure into a monoid, and combine the results with (<>).
    -- This fold is right-associative and lazy in the accumulator.
    foldMap :: (Monoid m) => (a -> m) -> Levelorder a -> m
    foldMap _ (LevelO Nil) = mempty
    foldMap f (LevelO (Branch l x r)) = foldMap f (nodesList [l, r] [x]) where
        nodesList [] xs = xs
        nodesList (Nil : ns) xs = nodesList ns xs
        nodesList ((Branch l x r) : ns) xs = nodesList (ns ++ [l, r]) (xs ++ [x])

    -- foldMap :: (Monoid m) => (a -> m) -> Levelorder a -> m
    -- foldMap _ (LevelO Nil) = mempty
    -- foldMap f (LevelO t) = let (m, _) = thread mempty t in m where
    --     thread m Nil = (m, ())
    --     thread m (Branch l x r) = ((f x) <> m2, ()) where
    --         (m1, _) = thread m l
    --         (m2, _) = thread m1 r

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
test25 = foldr (:) [] tree -- [1,2,3,4] -- in-order
test26 = foldr (:) [] $ PreO tree -- [3,1,2,4] -- pre-order
test27 = foldr (:) [] $ PostO tree -- [2,1,4,3] -- post-order
test28 = foldr (:) [] $ LevelO tree -- [3,1,4,2] -- BFS or level-order

tagBfs :: (Monoid m) => (a -> m) -> Tree a -> Tree m
tagBfs f t = let
    (ms, r) = thread (mempty : ms) t
        in r where
            thread ms Nil = (ms, Nil)
            thread (m : ms) (Branch l x r) =
                let (ms1, l') = thread ms l
                    (ms2, r') = thread ms1 r
                in ((m <> f x) : ms2, Branch l' m r')


-- f :: [Int] -> Maybe Bool
-- f = Just . getAny . foldMap Any . fmap even

-- g :: [Maybe a] -> Maybe a
-- g = getLast . foldMap Last

-- h :: [Int] -> Maybe Bool
-- h = Just . getAll . foldMap All . map isDigit

infiniteTree n = Branch (infiniteTree $ n + 1) n (Branch Nil 42 Nil)
-- test29 = elem 42 $ LevelO (infiniteTree 100)


{--
Реализуйте функцию

mkEndo :: Foldable t => t (a -> a) -> Endo a

принимающую контейнер функций и 
последовательно сцепляющую элементы этого контейнера с помощью композиции,
порождая в итоге эндоморфизм.

GHCi> e1 = mkEndo [(+5),(*3),(^2)]
GHCi> appEndo e1 2
17
GHCi> e2 = mkEndo (42,(*3))
GHCi> appEndo e2 2
6
--}
mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

e1 = mkEndo [(+5),(*3),(^2)]
test30 = appEndo e1 2 -- 17
e2 = mkEndo (42,(*3))
test31 = appEndo e2 2 -- 6


{--
Сделайте тип

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

представителем класса типов `Foldable` при условии, 
что аргументы композиции являются представителями `Foldable`.

GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
3
GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
7

ghci> :t Cmps
Cmps
     f (g a) -> (|.|) f g a
ghci> :t getCmps
getCmps
     (|.|) f g a -> f (g a)

--}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    -- Map each element of the structure into a monoid, and combine the results with (mappend <>).
    -- This fold is right-associative and lazy in the accumulator.
    foldMap :: (Monoid m) => (a -> m) -> (|.|) f g a -> m
    foldMap f cmps = foldMap (foldMap f) fga where -- два барьера, два фолдмэпа
        fga = getCmps cmps

    -- foldr :: (a -> b -> b) -> b -> (|.|) f g a -> b
    -- foldr f ini cmps = undefined

test32 = maximum $ Cmps [Nothing, Just 2, Just 3] -- 3
test33 = length $ Cmps [[1,2], [], [3,4,5,6,7]] -- 7
