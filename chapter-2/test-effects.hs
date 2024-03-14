{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
{-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
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
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use :" #-}

module TestEffects where

import Text.Parsec (getParserState)
import Data.Char ( toLower, isDigit, )
import Data.Function ( (&), )

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..),
    getAny, Any(..), getLast, Last(..), getAll, All(..), (<>)
    )

-- import Control.Monad.Identity ( Identity(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor ((<&>))

import Control.Applicative (
    Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>), liftA3, liftA2,
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
    even, Bool(..), odd,
    Monad(..),
    )

test = foldr (+) 0 [0..42]
test2 = foldr (*) 3 (Just 14)
test3 = foldr (*) 3 (Right 14)
test4 = foldr (*) 3 (13, 14)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5
{--
instance Foldable Tree where
    foldr f ini Nil = ini
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = foldr f ini r -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order
--}
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

data ResultL a = ErrorL | OkL a deriving (Eq, Show) -- sum-type, Maybe isomorph
instance Functor ResultL where fmap = fmapDefault -- используя траверс
instance Foldable ResultL where foldMap = foldMapDefault -- используя траверс

instance Traversable ResultL where
-- instance (Functor Result, Foldable Result) => Traversable Result where
    -- traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
    traverse _ ErrorL = pure ErrorL
    -- traverse f (Ok x) = (pure Ok) <*> (f x)
    traverse f (OkL x) = OkL <$> f x

test18 = traverse (\x -> [x+10, x+20]) (OkL 5)

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


{--
Реализуйте функцию

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]

работающую с эффектами как `traverse_`
но параллельно с накоплением эффектов 
«восстанавливающую» сворачиваемую структуру в виде списка:

GHCi> traverse2list (\x -> [x+10,x+20]) [1,2,3]
[[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
GHCi> traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)
[[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]
--}
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\ x y -> pure (:) <*> (f x) <*> y) (pure [])
-- все наши сиквенсы и траверсы сделаны через foldr, поэтому функция свертки формирует список,
-- комбинируя аппликативы через `apply over`

-- traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
-- traverse_ f = foldr ((*>) . f) (pure ())

-- sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
-- sequenceA2list = foldr (\ x y -> pure (:) <*> x <*> y) (pure [])

-- sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
-- sequenceA_ = foldr (*>) (pure ())

test34 = traverse2list (\x -> [x+10,x+20]) [1,2,3] -- [[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
test35 = traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil) -- [[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]


{--
Сделайте тип `Triple`

data Triple a = Tr a a a  deriving (Eq,Show)

представителем класса типов `Traversable`

GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
Right (Tr 12 14 16)
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
Left 8
GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)
--}

-- data Triple a = Tr a a a  deriving (Eq,Show)
instance Traversable Triple where
    traverse :: (Applicative f)=> (a -> f b) -> (Triple a) -> f (Triple b)
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z) -- см. функтор, плюс протаскивание конструктора в аппликатив

instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure :: a -> Triple a
    pure x = Tr x x x
    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (Tr fx fy fz) <*> (Tr x y z) = Tr (fx x) (fy y) (fz z)
{--

instance Foldable Triple where
    foldMap f (Tr x y z) = (f x) `mappend` ((f y) `mappend` (f z))
--}
test36 = foldl (++) "!!" (Tr "ab" "cd" "efg") -- "!!abcdefg"
test37 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16) -- Right (Tr 12 14 16)
test38 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4) -- Left 8
test39 = sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9)) -- Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)


{--
Сделайте тип данных `Result`

data Result a = Ok a | Error String deriving (Eq, Show)

представителем класса типов `Traversable` (и всех других необходимых классов типов).

GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
[Ok 7,Ok 3]
GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
[Error "!!!"]
--}

data Result a = Ok a | Error String
    deriving (Eq, Show)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Error s) = Error s
    fmap f (Ok x) = Ok $ f x

instance Applicative Result where
    pure :: a -> Result a
    pure = Ok
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (Ok f) <*> (Ok x) = Ok $ f x
    (Error s) <*> _ = Error s -- ошибки можно склеивать, не надо?
    _ <*> (Error s) = Error s

instance Foldable Result where
    foldMap :: Monoid m => (a -> m) -> Result a -> m
    foldMap _ (Error s) = mempty
    foldMap f (Ok x) = f x

instance Traversable Result where
    traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse f (Error s) = pure $ Error s
    traverse f (Ok x) = Ok <$> f x

test40 = traverse (\x->[x+2,x-2]) (Ok 5) -- [Ok 7,Ok 3]
test41 = traverse (\x->[x+2,x-2]) (Error "!!!") -- [Error "!!!"]


{--
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

представителем класса типов `Traversable` (а также всех других необходимых классов типов).

GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
Right (Branch (Branch Nil 1 Nil) 3 Nil)
GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
Left 2
GHCi> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
[Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
--}

-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)
{--
instance Traversable Tree where
    sequenceA :: (Applicative f)=> Tree (f a) -> f (Tree a)
    sequenceA Nil = pure Nil
    sequenceA (Branch l m r) = Branch <$> (sequenceA l) <*> m <*> (sequenceA r)
--}
{--
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldr f ini Nil = ini
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = foldr f ini r -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order
--}
test42 = traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil) -- Right (Branch (Branch Nil 1 Nil) 3 Nil)
test43 = traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil) -- Left 2
test44 = sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil -- [Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]


{--
Сделайте тип `Cmps`

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

представителем класса типов `Traversable` при условии, 
что аргументы композиции являются представителями `Traversable`

GHCi> sequenceA (Cmps [Just (Right 2), Nothing])
Right (Cmps {getCmps = [Just 2,Nothing]})
GHCi> sequenceA (Cmps [Just (Left 2), Nothing])
Left 2
--}
-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)
instance (Traversable a, Traversable b)=> Traversable (a |.| b) where
    -- traverse :: (Applicative f) => (x -> f y) -> (|.|) a b x -> f ((|.|) a b y)
    traverse f (Cmps x) = Cmps <$> traverse (traverse f) x -- протащить через два барьера

instance (Functor f, Functor g)=> Functor (f |.| g) where
    -- fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

{--
instance (Foldable a, Foldable b) => Foldable (a |.| b) where
    foldMap = (. getCmps) . foldMap . foldMap

--}

test45 = sequenceA (Cmps [Just (Right 2), Nothing]) -- Right (Cmps {getCmps = [Just 2,Nothing]})
test46 = sequenceA (Cmps [Just (Left 2), Nothing]) -- Left 2


{--
Рассмотрим следующий тип данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

Этот тип представляет собой контейнер-последовательность, который по построению может содержать только нечетное число элементов:

GHCi> cnt1 = Un 42
GHCi> cnt3 = Bi 1 2 cnt1
GHCi> cnt5 = Bi 3 4 cnt3
GHCi> cnt5
Bi 3 4 (Bi 1 2 (Un 42))
GHCi> cntInf = Bi 'A' 'B' cntInf
GHCi> cntInf
Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'Interrupted.
GHCi>

Сделайте этот тип данных представителем классов типов `Functor`, `Foldable` и `Traversable`:

GHCi> (+1) <$> cnt5
Bi 4 5 (Bi 2 3 (Un 43))
GHCi> toList cnt5
[3,4,1,2,42]
GHCi> sum cnt5
52
GHCi> traverse (\x->[x+2,x-2]) cnt1
[Un 44,Un 40]
--}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)
{--
instance Functor OddC where
    fmap :: (a -> b) -> OddC a -> OddC b
    fmap f (Un x) = Un (f x)
    fmap f (Bi x y rest) = Bi (f x) (f y) (fmap f rest)
--}
instance Foldable OddC where
    foldMap :: (Monoid m)=> (a -> m) -> OddC a -> m
    foldMap f (Un x) = f x
    foldMap f (Bi x y rest) = f x <> f y <> (foldMap f rest)

instance Traversable OddC where
    sequenceA :: (Applicative f)=> OddC (f a) -> f (OddC a)
    sequenceA (Un x) = Un <$> x
    sequenceA (Bi x y rest) = Bi <$> x <*> y <*> (sequenceA rest)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf
test47 = cnt5 -- Bi 3 4 (Bi 1 2 (Un 42))

test48 = (+1) <$> cnt5 -- Bi 4 5 (Bi 2 3 (Un 43))
test49 = toList cnt5 -- [3,4,1,2,42]
test50 = sum cnt5 -- 52
test51 = traverse (\x->[x+2,x-2]) cnt1 -- [Un 44,Un 40]


{--
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

представителем класса типов `Traversable` таким образом, 
чтобы обеспечить для `foldMapDefault` порядок обхода «postorder traversal»:

GHCi> testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
GHCi> foldMapDefault (\x -> [x]) testTree
[1,3,2,5,4]
--}

-- solution
-- надо либо traverse определять, либо sequenceA и fmap

-- import Data.Traversable (foldMapDefault)
-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
    foldMap = foldMapDefault
    -- foldMap f Nil = mempty
    -- foldMap f (Branch l x r) = (foldMap f l) <> (foldMap f r) <> (f x)

instance Traversable Tree where
    traverse :: (Applicative f)=> (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Nil) = pure Nil
    -- traverse f (Branch l m r) = Branch <$> (traverse f l) *> (traverse f r) <* (f m) -- test #3 failed
    -- traverse f (Branch l m r) = (\ x y z -> Branch x z y) <$> (traverse f l) <*> (traverse f r) <*> (f m)
    traverse f (Branch l m r) = (flip . Branch) <$> (traverse f l) <*> (traverse f r) <*> (f m)

{--
instance Traversable Tree where
    sequenceA :: (Applicative f)=> Tree (f a) -> f (Tree a)
    sequenceA Nil = pure Nil
    sequenceA (Branch l m r) = Branch <$> (sequenceA l) <*> m <*> (sequenceA r)
--}

-- foldMapDefault f = getConst . traverse (Const . f)
testTree2 = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
test52 = foldMapDefault (\x -> [x]) testTree2 -- [1,3,2,5,4]


{--
Сделайте парсер `PrsE` (из первого модуля курса) представителем класса типов Monad

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
Right (('A','B'),"C")
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
Left "unexpected C"
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
Left "unexpected B"
--}
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }
instance Monad PrsE where
  (>>=) :: PrsE a -> (a -> PrsE b) -> PrsE b
  (PrsE a) >>= k = PrsE b where
    b str = do -- either monad
        (x1, s1) <- a str -- left
        (x2, s2) <- runPrsE (k x1) s1 -- right
        return (x2, s2)

instance Functor PrsE where
  fmap :: (a -> b) -> PrsE a -> PrsE b
  fmap a2b (PrsE parsA) = PrsE parsB where
    parsB str = do -- either monad
        (a, rest) <- parsA str
        return (a2b a, rest)

instance Applicative PrsE where
  pure :: a -> PrsE a
  pure a = PrsE (\s -> return (a, s))

  (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
  (PrsE parsA2b) <*> (PrsE parsA) = PrsE parsB where
    parsB str = do -- either monad
        (a2b, s1) <- parsA2b str -- left
        (a, s2) <- parsA s1 -- right
        return (a2b a, s2) -- combine

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pred = PrsE fun where
    fun "" = Left "unexpected end of input"
    fun (c:cs) = if pred c then Right (c, cs) else Left ("unexpected " ++ [c])

test53 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC" -- Right (('A','B'),"C")
test54 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD" -- Left "unexpected C"
test55 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD" -- Left "unexpected B"


{--
Для типа данных `OddC`
(контейнер-последовательность, который по построению может содержать только нечетное число элементов)

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

реализуйте функцию

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a

конкатенирующую три таких контейнера в один:

GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concat3OC tst1 tst2 tst3
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров `OddC`.
Реализуйте всё «честно», не сводя к стандартным спискам.
--}
-- data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) rest = Bi x y rest
concat3OC (Un x) (Bi y1 y2 ys) rest = Bi x y1 (concat3OC (Un y2) ys rest)
concat3OC (Bi x1 x2 xs) ys rest = Bi x1 x2 (concat3OC xs ys rest)

tst1 = Bi 'a' 'b' (Un 'c') -- 
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
test56 = concat3OC tst1 tst2 tst3 -- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))


{--
Для типа данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

реализуйте функцию

concatOC :: OddC (OddC a) -> OddC a

Она должна обеспечивать для типа `OddC` поведение, аналогичное поведению функции `concat` для списков:

GHCi> concatOC $ Un (Un 42)
Un 42
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

Реализуйте всё «честно», не сводя к стандартным спискам.
--}
-- data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un xs) = xs
concatOC (Bi xs ys zs) = concat3OC xs ys (concatOC zs)

test57 = concatOC $ Un (Un 42) -- Un 42
tst12 = Bi 'a' 'b' (Un 'c')
tst22 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst32 = Bi 'i' 'j' (Un 'k')
test58 = concatOC $ Bi tst12 tst22 (Un tst32) -- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))


{--
Сделайте тип данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

представителем классов типов `Functor`, `Applicative` и `Monad`. 
Семантика должна быть подобной семантике представителей этих классов типов для списков: 
монада `OddC` должна иметь эффект вычисления с произвольным нечетным числом результатов:

GHCi> tst1 = Bi 10 20 (Un 30)
GHCi> tst2 = Bi 1 2 (Bi 3 4 (Un 5))
GHCi> do {x <- tst1; y <- tst2; return (x + y)}
Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
GHCi> do {x <- tst2; y <- tst1; return (x + y)}
Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))

Функцию `fail` можно не реализовывать, полагаясь на реализацию по умолчанию.
Реализуйте всё «честно», не сводя к стандартным спискам.
--}

instance Functor OddC where
    fmap :: (a -> b) -> OddC a -> OddC b -- (Functor f)=>    (a -> b) -> f a -> f b -- infixl 4 <$>, fmap
    fmap f (Un x) = Un (f x)
    fmap f (Bi x y rest) = Bi (f x) (f y) (fmap f rest)

instance Applicative OddC where
    pure :: a -> OddC a
    pure = Un

    (<*>) :: OddC (a -> b) -> OddC a -> OddC b -- (Applicative f)=>  f (a -> b) -> f a -> f b -- infixl 4 <*>
    (Un f) <*> xs = fmap f xs
    (Bi f g restF) <*> xs = concat3OC fx gx restX where
        fx = fmap f xs
        gx = fmap g xs
        restX = restF <*> xs

instance Monad OddC where
    (>>=) :: OddC a -> (a -> OddC b) -> OddC b -- (Monad m)=>  m a -> (a -> m b) -> m b -- infixl 1 >>=
    (Un x) >>= k = k x
    (Bi x y rest) >>= k = concat3OC kx ky kRest where
        kx = k x
        ky = k y
        kRest = rest >>= k

tst13 = Bi 10 20 (Un 30)
tst23 = Bi 1 2 (Bi 3 4 (Un 5))
test59 = do {x <- tst13; y <- tst23; return (x + y)} -- Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
test60 = do {x <- tst23; y <- tst13; return (x + y)} -- Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))

{--
instance Foldable OddC where
    -- (Foldable t, Monoid m)=>    (a -> m) -> t a -> m
    foldMap f (Un x) = f x
    foldMap f (Bi x y r) = f x <> f y <> foldMap f r

instance Traversable OddC  where
    -- (Traversable t, Applicative f)=> t (f a) -> f (t a)
    sequenceA (Un x) = Un <$> x
    sequenceA (Bi x y r) = liftA3 Bi x y (sequenceA r)
    -- sequenceA (Bi x y rest) = Bi <$> x <*> y <*> (sequenceA rest)
--}
