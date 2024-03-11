# FP Haskell, chapter 2, управление эффектами

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-2/test-effects.hs)

## Мотивация

Обобщение сверток: Foldable.
Traversable: как-бы-свертка но с сохранением структуры.
Applicative vs Monad: нюансы работы с эффектами; порядок вычислений (направление пайплайна).
Applicative-Alternative vs Monad-MonadPlus: свойства моноида-суммы для монад, сравнение с альтернативом (для аппликатива).

## definitions

- Foldable
- Endo (monoid)
- Dual (monoid)
- Traversable
- MonadFail
- MonadPlus

```hs
class Foldable t where -- MCD (minimal complete definition foldMap | foldr)
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого `a` в `m` с последующим "суммированием"
    foldr :: (a -> b -> b) -> b -> t a -> b -- foldr f ini [1, 2, 3] = 1 f (2 f (3 f ini))
    foldl :: (b -> a -> b) -> b -> t a -> b -- foldl f ini [1, 2, 3] = ((ini f 1) f 2) f 3
    fold :: Monoid m => t m -> m -- свертка "списка" моноидов

newtype Endo a -- The monoid of endomorphisms under composition
instance Monoid (Endo a) where
    mempty = Endo id
    mappend (Endo f) (Endo g) = Endo (f . g) -- эф после же

newtype Dual a = Dual { getDual :: a }
instance (Monoid a) => Monoid (Dual a) where
    mempty = Dual mempty
    mappend (Dual x) (Dual y) = Dual (mappend y x) -- n.b. перевернули порядок следования при склейке

fold :: (Foldable t, Monoid m) => t m -> m -- свертка "списка" моноидов
fold = foldr mappend mempty

asum :: (Foldable t, Alternative f) => t (f a) -> f a -- аппликативный функтор с моноидальной структурой: альтернатив
asum = foldr (<|>) empty -- fold для альтернатива (сворачивается контекст, не значения)

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ()) -- фолд эффектов аппликатива

-- fold vs foldMap, sequenceA_ vs traverse_
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

class (Functor t, Foldable t) => Traversable t where -- glorified functor -- Minimal Complete Definition `traverse | sequenceA`
    sequenceA :: Applicative f => t (f a) -> f (t a) -- "список" аппликативов преобразовать в аппликатив "списка"
    sequenceA = traverse id
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = sequenceA . fmap -- "список" `a` преобразовать в аппликатив "списка" `b`, используя "лифт" `a -> f b`

```
definitions

## chapter 2.1, Класс типов Foldable

https://stepik.org/lesson/30427/step/1?unit=11044

- Обобщение сверток списков на произвольные контейнеры
- Свертки простых контейнеров
- Свертки деревьев
- Свертки моноидов
- Полное определение класса Foldable
- Полное определение класса Foldable (продолжение)
- Эндоморфизмы
- foldr через foldMap
- foldl через foldMap

### 2.1.2 foldr, foldl

МЫ уже видели функции свертки `foldr, foldl`, для списков.
Почему бы не сделать их более общими, для любого контейнера (контекста),
обладающего неким интерфейсом итерирования.
```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b

foldr f ini [1, 2, 3] = 1 f (2 f (3 f ini))
foldl f ini [1, 2, 3] = ((ini f 1) f 2) f 3

-- понадобится для каждого контейнера определить порядок перебора (линеаризации, сериализации) значений,
-- чтобы последовательная свертка a -> b -> b (или b -> a -> b) давала нужный эффект по завершению.

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
-- t это конструктор типов, кайнд * -> *

Minimal complete definition: `foldMap | foldr`
```
repl

### 2.1.3 Foldable (`[]`, `Maybe`, `(,)`)

Реализации фолдабл для некоторых типов
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

instance Foldable [] where
    foldr f ini [] = ini
    foldr f ini (x:xs) = f x (foldr f ini xs)
    foldl f ini [] = []
    foldl f ini (x:xs) = foldl f (f ini x) xs

instance Foldable Maybe where
    foldr f ini Nothing = Nothing
    foldr f ini (Just x) = f x ini
    foldl f ini Nothing = Nothing
    foldl f ini (Just x) = f ini x

ghci> foldr (*) 3 (Just 14)
42
ghci> foldr (*) 3 Nothing
3
-- для Either похожим образом
ghci> foldr (*) 3 (Right 14)
42
ghci> foldr (*) 3 (Left 14)
3

-- для пары тоже, первый элемент игнорируется, но по другим причинам
ghci> foldr (*) 3 (13, 14)
42
-- конструктор типов пары двух-параметрический, поэтому свободным остается второй параметр пары, с ним и работаем

instance Foldable ((,) c) where
    foldr f ini (_, x) = f x ini
```
repl

### 2.1.4 test

```hs
{--
Сделайте тип

data Triple a = Tr a a a  deriving (Eq, Show)

представителем класса типов `Foldable`:

GHCi> foldr (++) "!!" (Tr "ab" "cd" "efg")
"abcdefg!!"
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
--}

-- solution

-- data Triple a = Tr a a a  deriving (Eq, Show)
instance Foldable Triple where
    -- foldr :: (a -> b -> b) -> b -> Triple a -> b
    -- foldr f ini (Tr x y z) = f x (f y (f z ini))
    -- foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))

    -- foldMap :: (Monoid m) => (a -> m) -> Triple a -> m
    foldMap f (Tr x y z) = (f x) `mappend` ((f y) `mappend` (f z))
    -- Map each element of the structure into a monoid, and combine the results with (<>).
    -- This fold is right-associative and lazy in the accumulator.

```
test

### 2.1.5 Foldable Tree

Свертка дерева уже интереснее. Разные варианты обхода (in-order, pre-order, post-order)
```hs
import Data.Foldable
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5

instance Foldable Tree where -- sum-type, pat.mat.
    foldr f ini Nil = ini -- first case
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = (foldr f ini r) -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

ghci> treeToList testTree 
[4,2,1,3,5] -- pre-order

ghci> treeToList testTree 
[1,2,3,4,5] -- in-order (в порядке сортировки)
```
repl

### 2.1.6 test

```hs
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
--}

-- solution
{--
https://en.wikipedia.org/wiki/Tree_traversal
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

           3
         /   \
        1     4
         \
          2
--}
-- data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
instance Foldable Tree where
   foldr f ini Nil = ini
   foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order, LNR

-- newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
    -- foldr :: (a -> b -> b) -> b -> Preorder a -> b
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f iniR (PreO l)) where iniR = foldr f ini (PreO r) -- pre-order, NLR

-- newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
    -- foldr :: (a -> b -> b) -> b -> Postorder a -> b
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f foldedRight (PostO l)  where -- post-order, LRN
        foldedRight = foldr f foldedNode (PostO r) -- RN
        foldedNode = f x ini

-- newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
instance Foldable Levelorder where
    -- Map each element of the structure into a monoid, and combine the results with (<>).
    -- This fold is right-associative and lazy in the accumulator.
    -- foldMap :: (Monoid m) => (a -> m) -> Levelorder a -> m
    foldMap _ (LevelO Nil) = mempty
    foldMap f (LevelO (Branch l x r)) = foldMap f (nodesList [l, r] [x]) where
        nodesList [] xs = xs
        nodesList (Nil : ns) xs = nodesList ns xs
        nodesList ((Branch l x r) : ns) xs = nodesList (ns ++ [l, r]) (xs ++ [x])

-- alternatives

import Data.Monoid

instance Foldable Tree where foldMap = foldMapInt (\l m r -> l <> m <> r) -- point-free -- in-order, LNR

instance Foldable Preorder where foldMap = (. \(PreO x) -> x) . foldMapInt (\l m r -> m <> l <> r) -- pre-order, NLR

instance Foldable Postorder where foldMap = (. \(PostO x) -> x). foldMapInt (\l m r -> l <> r <> m) -- post-order, LRN

instance Foldable Levelorder where -- BFS
  foldMap f (LevelO t) = foldMap id $ go t where
    go Nil            = []
    go (Branch l m r) = f m : zipl (go l) (go r) -- monoidal-zip: какова картина: зип двух веток дерева, во голова

-- hidden (in foldMapInt) constraint: Monoid, it's a cheat!
foldMapInt::Monoid b=> (b->b->b->b) -> (a->b) -> Tree a -> b
foldMapInt g f = go where
  go Nil            = mempty -- :: Tree -> b
  go (Branch l m r) = g (go l) (f m) (go r)

zipl::Monoid a=> [a] -> [a] -> [a]
zipl [] x           = x
zipl x []           = x
zipl (x:xs) (y:ys) = x <> y : (zipl xs ys ) -- monoidal-zip

-------------------------------------------------------------------------

import Data.Monoid ((<>))

instance Foldable Tree where
   foldMap f Nil = mempty
   foldMap f (Branch l x r) = (foldMap f l) <> (f x) <> (foldMap f r)

instance Foldable Preorder where
   foldMap f (PreO Nil) = mempty
   foldMap f (PreO (Branch l x r)) = (f x) <> (foldMap f (PreO l)) <> (foldMap f (PreO r))

instance Foldable Postorder where
   foldMap f (PostO Nil) = mempty
   foldMap f (PostO (Branch l x r)) = (foldMap f (PostO l)) <> (foldMap f (PostO r)) <> (f x)

instance Foldable Levelorder where
   foldMap f tree = helper f tree [] where
      helper f (LevelO Nil) [] = mempty 
      helper f (LevelO Nil) (q:qs) = helper f q qs
      helper f (LevelO (Branch l x r)) [] = (f x) <> (helper f (LevelO l) [LevelO r])
      helper f (LevelO (Branch l x r)) (q:qs) = (f x) <> (helper f q (qs ++ [LevelO l, LevelO r]))

------------------------------------------------------------------------

instance Foldable Tree where
--foldr :: (a -> b -> b) -> b -> t a -> b  
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l 
  

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l)) 

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = (\i -> (foldr f i (PostO l))) . (\i -> (foldr f i (PostO r))) . f x $ ini 

instance Foldable Levelorder where
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO tr) = foldr f ini vals
    where
      vals = concat (levels tr)
      
      levels :: Tree a -> [[a]]
      levels tr = f tr [] where
        f (Branch l x r) (y:ys) = (x:y) : foldr f ys [l,r]
        f (Branch l x r) []     = [x]   : foldr f [] [l,r]
        f (Nil) (y:ys) = (y:ys)
        f (Nil) []     = []

-----------------------------------------------------------------------------

ii f a c b = f a b c

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch l x r) =
    ii foldr f l
    . f x
    . ii foldr f r
    $ ini

instance Foldable Preorder where
  foldr _ ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) =
    f x
    . ii foldr f (PreO l)
    . ii foldr f (PreO r)
    $ ini

instance Foldable Postorder where
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) =
    ii foldr f (PostO l)
    . ii foldr f (PostO r)
    . f x
    $ ini

instance Foldable Levelorder where
  foldr f ini (LevelO tree) = 
    let g [] xs = xs
        g ts xs = g (foldMap branches ts) $ xs ++ foldMap values ts

        branches Nil = []
        branches (Branch l x r) = [l, r]

        values Nil = []
        values (Branch _ x _) = [x]
    in foldr f ini $ g [tree] []

------------------------------------------------------------------------

g :: Foldable t => (a -> b -> b) -> t a -> b -> b
g = flip . foldr

instance Foldable Tree where
  foldr f ini (Branch l a r) =
    g f l $ f a $ g f r ini
  foldr _ ini _ = ini

instance Foldable Preorder where
  foldr f ini (PreO (Branch l a r)) =
    f a $ g f (PreO l) $ g f (PreO r) ini
  foldr _ ini _ = ini

instance Foldable Postorder where
  foldr f ini (PostO (Branch l a r)) =
    g f (PostO l) $ g f (PostO r) $ f a ini
  foldr _ ini _ = ini

instance Foldable Levelorder where
  foldr f ini (LevelO tree) =
    foldr f ini $ flatten [tree]

flatten :: [Tree a] -> [a]
flatten [] = []
flatten (Nil : xs) = flatten xs
flatten ((Branch l a r) : xs) = a : flatten (xs ++ [l, r])

-------------------------------------------------------------------------

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
    foldr f ini (LevelO t) = foldl (flip f) ini (snd $ treeToList ([t], []))
        where
            treeToList ([], vs) = ([], vs)
            treeToList (ts, vs) = treeToList (foldr node ([], vs) ts)
                where
                    node Nil x = x
                    node (Branch l x r) (trees, vs') = (r : l : trees, x : vs')

------------------------------------------------------------------------------

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l
    
instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))
    
instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)
    
instance Foldable Levelorder where
    foldr f ini (LevelO tree) = foldr f ini (rip [tree] []) where
        rip [] s = s
        rip (Nil : trees) s = rip trees s
        rip ((Branch l a r): trees) s = rip (trees ++ [l, r]) (s ++ [a])

---------------------------------------------------------------------------

current Nil = Nothing
current (Branch l n r) = Just n

siblings Nil = []
siblings (Branch l n r) = [l, r]

instance Foldable Levelorder where
    foldr f ini (LevelO t) = foldr f ini $ mapMaybe current $ concat $ takeWhile (not . null) $ iterate (concatMap siblings) [t]

-------------------------------------------------------------------------

instance Foldable Levelorder where
  foldr f ini tree = foldr' (singleton tree) where
    foldr' seq | null seq  = ini
    foldr' seq | otherwise = foldr (\v ini -> f v ini) retVal maybeVal where
      retVal = foldr' seq'
      (seq', maybeVal) = processNode (1 `drop` seq) (seq `index` 0)
      processNode seq (LevelO Nil)            = (seq, Nothing)
      processNode seq (LevelO (Branch r v l)) =  (seq |> (LevelO r) |> (LevelO l), Just v)


```
test

### 2.1.7 Foldable (fold, foldMap)

У Foldable есть (кроме foldl, foldr) другие функции.
Некоторые требуют чтобы значения были в моноиде.
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

-- функции требующие от значения быть в моноиде:

    fold :: Monoid m => t m -> m -- свертка "списка" моноидов
    fold = foldr mappend mempty -- mconcat фактически

    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого `a` в `m` с последующим суммированием
    -- foldMap f ta = let tm = fmap f ta in fold tm -- увы, так нельзя, тип t не заявлен как Functor, у него нет fmap
    foldMap f ta = foldr (mappend . f) mempty ta -- дефолтная реализация через `foldr`
    foldMap f = foldr (mappend . f) mempty -- pointfree
-- mappend . f -- дает нам нужную для foldr функцию двух аргументов

-- examples

ghci> fold [[1,2,3],[4,5]]
[1,2,3,4,5]

ghci> foldMap Sum [1,2,3,4]
Sum {getSum = 10}
ghci> :t Sum
Sum :: a -> Sum a

ghci> foldMap Product [1,2,3,4]
Product {getProduct = 24}

ghci> foldMap Sum testTree 
Sum {getSum = 15}

ghci> treeToList testTree 
[1,2,3,4,5] -- in-order
ghci> foldMap Product testTree 
Product {getProduct = 120} -- 5!
```
repl

### 2.1.8 test

```hs
{--
Предположим, что определены следующие функции

f = Just . getAny . foldMap Any . fmap even
g = getLast . foldMap Last
h = Just . getAll . foldMap All . map isDigit

Сопоставьте их вызовы и результаты этих вызовов.
Предполагается, что загружены все модули, требующиеся для доступа к использованным функциям и конструкторам данных.

f [3,5,6] = ?
h [3,5,6] = ?
g [Just True,Just False,Nothing] = ?

Just True, Just False, error ...
--}

-- solution: поведение моноидов Any, Last, All

ghci> f [3,5,6]
Just True
-- f = Just . getAny . (foldMap Any) . (fmap even)
fmap even -- из списка чисел даст список бул [Fase, False, True]
foldMap Any -- из списка сделает моноид-обертку бул (mappend False True = True)
-- ghci> Any False `mappend` Any True
-- Any {getAny = True}
getAny -- развернет обертку в бул значение True
Just -- завернет бул в мэйби

ghci> h [3,5,6]
<interactive>:4:4: error:     • No instance for (Num Char) arising from the literal ‘3’
-- h = Just . getAll . (foldMap All) . (map isDigit)
map isDigit -- сломается сразу, числа не символы

ghci> g [Just True,Just False,Nothing]
Just False
-- g = getLast . (foldMap Last)
foldMap Last -- из списка мэйби сделает одно значение, `Maybe monoid returning the rightmost non-Nothing value`: Just False
getLast -- развернет обертку Last: Just False

```
test

### 2.1.9 Foldable (sum, product, null, toList, length, maximum, minimum, elem)

Посмотрим на интерфейс, получаемый от Foldable
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    fold :: Monoid m => t m -> m -- свертка "списка" моноидов
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого `a` в `m` с последующим "суммированием"

-- другие полезные функции

    sum :: Num a => t a -> a
    sum = getSum . foldMap Sum -- с использованием моноида

    product :: Num a => t a -> a
    product = getProduct . foldMap Product -- моноид продукта

    null :: t a -> Bool
    null = foldr (\ _ _ -> False) True -- трюк: лямбда сработает на первом же элементе и даст не-нулл

    toList :: t a -> [a]
    toList = foldr (:) [] -- протаскивание конструктора по элементам

    length :: t a -> Int
    length = _ -- строгая версия левой свертки

    maximum
    minimum
    elem

-- у пары первый элемент не играет в фолдабл (тайпкласс на одно-параметрических типах)
ghci> maximum (99, 42)
42

ghci> maximum [] -- не тотальная функция
*** Exception: Prelude.maximum: empty list
```
repl

### 2.1.10 test

```hs
{--
Предположим, что у нас реализованы все свертки, 
основанные на разных стратегиях обхода дерева из предыдущей задачи. 
Какой из вызовов «лучше определен», то есть возвращает результат на более широком классе деревьев?

Select one option from the list
- elem 42 someTree
- elem 42 $ PreO someTree
- elem 42 $ PostO someTree
- elem 42 $ LevelO someTree
--}

-- solution
-- Эта задача про разницу между DFS и BFS, остальное вторично.
-- DFS пойдет по бесконечной ветке и никогда не найдет значение, которое находится в самом начале другой ветки.
-- BFS проходит по всем веткам и сможет найти значение, если оно есть, не уходя вглубь бесконечной ветки.

-- функия возвращает True при наличии элемента в фолдабле
ghci> :i elem
class Foldable t where
  elem :: Eq a => a -> t a -> Bool -- Defined in ‘Data.Foldable’
-- сделано через any, виснет на бесконечности
any :: Foldable t => (a -> Bool) -> t a -> Bool

-- т.е. вопрос можно переформулировать так: на каком дереве перебор не зависнет?
-- ответ:
elem 42 $ LevelO someTree
-- по факту, методом исключения: все остальные используют одинаковые стратегии вычислений, а выбрать надо один элемент
-- я вижу только одну разницу: три стратегии используют рекурсию и (на бесконечности) выжирают память,
-- одна стратегия может быть сделана с TCO и память не жрать.
-- Что не отменяет факта зависания программы.

-- можно методом тыка
GHCi> infiniteTree n = Branch (infiniteTree $ n + 1) n (Branch Leaf 42 Leaf)
GHCi> someTree = infiniteTree 100
-- это дерево демонстрирует наличие бесконечной ветки и искомое значение в другой ветке.

```
test

### 2.1.11 Foldable (foldr', foldr1, and, or, any, all, concat, concatMap)

Продолжим смотреть на предоставляемые фолдаблом функции
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    fold :: Monoid m => t m -> m -- свертка "списка" моноидов
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием

    sum :: Num a => t a -> a
    product :: Num a => t a -> a
    null :: t a -> Bool
    toList :: t a -> [a]
    length :: t a -> Int

    maximum
    minimum
    elem

-- другие полезные функции

    foldr' -- строгие свертки
    foldl'

    foldr1 -- свертка не-пустого контейнера, начальное значение свертки из контейнера
    foldl1

-- обобщение списочных функций для фолдабла
and
or
any
all

concat -- flatten -- для моноида это fold, но для списка есть специализированная реализация, оптимизированная
concatMap -- flatMap
```
repl

### 2.1.12 Endo - эндоморфизм aka `a -> a`

Хотим реализацию `foldr` через `foldMap`, ибо `MCD: foldMap | foldr`
а реализацию foldMap через foldr мы уже видели.
foldl реализуется через foldMap, кстати.

Для этого надо ...
Предварительно необходимо понять "эндоморфизм".
Эндоморфизм это стрелка из сета в этот же сет.
Область определений и область значений функции совпадают.
Это стрелка из А в А. (инт в инт, строка в строку, ...).

На типе `Endo` можно сделать моноид на операции "композиция": композиция функций как mappend aka `<>`.
```hs
-- Data.Monoid.Endo
newtype Endo a = Endo { appEndo :: a -> a }
-- одно-параметрический контейнер, упаковывает стрелку эндоморфизма

ghci> :t Endo (+2)
Endo (+2) :: Num a => Endo a
ghci> :t (+2)
(+2) :: Num a => a -> a
ghci> :t Endo
Endo :: (a -> a) -> Endo a
ghci> :t appEndo 
appEndo :: Endo a -> a -> a -- два аргумента или один аргумент и вернет стрелку
ghci> :t appEndo $ Endo (+2)
appEndo $ Endo (+2) :: Num a => a -> a

ghci> appEndo ( Endo (+2) ) 7
9

-- эндоморфизм это моноид, относительно операции композиции
instance Monoid (Endo a) where
    mempty = Endo id
    mappend (Endo f) (Endo g) = Endo (f . g) -- эф после же
-- композиция функций ассоциативна, mappend тоже ассоциативен: Endo это полугруппа
-- наличие нейтрали (id) дает нам уже моноид

ghci> appEndo (Endo (+2) `mappend` Endo (+3) `mappend` Endo(+4)) 1
10
ghci> appEndo (Endo (+2) <> Endo (+3) <> Endo(+4)) 1
10
```
repl

### 2.1.13 test

```hs
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
mkEndo = undefined

-- solution

import Data.Monoid ( Endo(..) )
mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo -- делаем моноид и композим (маппенд), при этом право-ассоциативно

```
test

### 2.1.14 реализация foldr через foldMap

foldMap реализован через foldr, foldr реализован через foldMap.
Достаточно предоставить свою реализацию любой из этих ф. чтобы сделать свой фолдабл.

Используем эндоморфизм для реализации foldr через foldMap:
заворачиваем элементы фолдабла в моноид-стрелку, потом композим.
```hs
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini ta = appEndo composedFunc ini where
        -- задача: нужен моноид для foldMap, это `Endo . f`
        composedFunc = foldMap (Endo . f) ta -- конструктор энду после эф, отправленный в фолд:
        -- завернет вычисление каждого элемента в функцию завернутую в энду, и сделает композицию этих функций (вложенные врапперы)
        -- грубо: превращает список элементов в список частично примененных функций, которые можно композить моноидально

-- Моноид, который нужен в foldMap: это энду, который просто стрелка.
-- Каждый элемент фолдабла завернут в стрелку.

ghci> :t Endo
Endo :: (a -> a) -> Endo a

ghci> :t appEndo 
appEndo :: Endo a -> a -> a

-- разбор устройства foldr-using-foldMap

ini :: b
f :: a -> (b -> b) -- для удобства рассуждений, `f` берет `a` и возвращает эндоморфизм
(Endo . f) :: a -> Endo b -- эта композиция выглядит как аргумент для foldMap: (a -> m)
foldMap (Endo . f) ta :: Endo b -- остается только подставить аргумент типа бэ и ... готово
-- по типам все подходит как надо, ок.

-- посмотрим на семантику foldr
foldr f ini [1,2,3]
= f 1 (f 2 (f 3 ini))
= f 1 ((f 2 . f 3) ini) -- crutial: переписали цепочки применений в виде композиции частично-примененных функций
= (f 1 . f 2 . f 3) ini) -- все частично-примененные функции дают эндоморфизмы (см. сигнатуру f :: a -> (b -> b))
= appEndo (Endo (f 1) <> Endo (f 2) <> Endo (f 3)) ini

{--
Своими словами:
для применения foldMap нужен моноид, Endo дает нам моноид,
суммирование (свертка) в этом моноиде дает нам композицию функций,
композиция набора функций (с захваченными элементами из оригинального фолдабл) применяется к затравке ini.
Вычисление протаскивается через цепочку (композицию) функций и вуаля.
--}

-- для справки: фолдл тоже сделан через фолдмэп
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
```
repl

### 2.1.15 реализация foldl через foldMap (Dual)

Чтобы сделать это, нам понадобится моноид `Dual`, сделанный как модификация любого моноида,
с перевернутым порядком `mappend`.
Дуальная коммутативность?
```hs
newtype Dual a = Dual { getDual :: a }

instance (Monoid a) => Monoid (Dual a) where
    mempty = Dual mempty
    mappend (Dual x) (Dual y) = Dual (mappend y x) -- n.b. перевернули порядок следования при склейке
-- любопытно, почему дуал а не реверс или флип
-- потому что позволяет получать дуальные моноиды, относительно порядка аппенда

-- examples

ghci> "Abc" <> "De" <> "Fgh"
"AbcDeFgh"
ghci> Dual "Abc" <> Dual "De" <> Dual "Fgh"
Dual {getDual = "FghDeAbc"} -- reverse append order

ghci> foldMap First [Nothing, Just 3, Just 5, Nothing]
First {getFirst = Just 3}
ghci> foldMap (Dual . First) [Nothing, Just 3, Just 5, Nothing] -- monoid Last, dual to First
Dual {getDual = First {getFirst = Just 5}}

ghci> appEndo (Endo (+5) <> Endo (*3)) 2
11 -- +5 . (2 *3)

ghci> (appEndo . getDual) ((Dual . Endo) (+5) <> (Dual . Endo) (*3)) 2
21 -- *3 . (2 +5)

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini ta = appEndo (foldMap (Endo . f) ta) ini

    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f ini ta = appEndo (getDual (foldMap (Dual . Endo . (flip f)) ta)) ini -- как и foldr только дуально-флипнутый

ghci> :t Endo
Endo :: (a -> a) -> Endo a
ghci> :t appEndo 
appEndo :: Endo a -> a -> a

ini :: b
f :: b -> a -> b
flip f :: a -> b -> b -- где (b -> b) это эндоморфизм
-- остальное вполне очевидно, с учетом уже сделанного разбора foldr
```
repl

### 2.1.16 test

```hs
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
--}

-- solution
-- эта затея про конструкторы типов (f g a), матрешка из "контейнера" `f`
-- внутри которого "контейнер" `g` внутри которого значение типа `a`.
-- Эти "контейнеры" реализуют фолдабл (foldr, foldMap).
-- Надо как-то протащить значения через два барьера "контейнеров"
-- как и функторами (монадами): "поднимаем" в первую оболочку, через фолдмап, функцию под применением второго фолдмапа,
-- для прохода через вторую оболочку

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 
instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    -- foldMap :: (Monoid m) => (a -> m) -> (|.|) f g a -> m
    foldMap f cmps = foldMap (foldMap f) fga where -- два барьера, два фолдмэпа
        fga = getCmps cmps

-- alternatives

instance (Foldable a, Foldable b) => Foldable (a |.| b) where
    foldMap = (. getCmps) . foldMap . foldMap
-- ghci> :t (.)
--      (b -> c) -> (a -> b) -> a -> c
-- ghci> :t (. getCmps)
--      (f (g a) -> c) -> (|.|) f g a -> c
-- ghci> :t getCmps
--      (|.|) f g a -> f (g a)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap f = (foldMap . foldMap) f . getCmps

```
test

## chapter 2.2, Класс типов Traversable

https://stepik.org/lesson/30428/step/1?unit=11045

- Foldable и Alternative: asum
- Foldable и Applicative: sequenceA_
- Foldable и Applicative: traverse_
- Ограниченность Foldable
- Определение класса Traversable
- Представители Traversable: Maybe
- Представители Traversable: пара
- Представители Traversable: список

### 2.2.2 asum (fold for Alternative)

Как можно сочетать `Applicative` (`Alternative`) и `Foldable`?
Рассмотрим такие сочетания, проанализируем и, в итоге, выведем `Traversable`.
Спойлер: использование свойств моноида для фоолд: нейтральный элемент и бинарная операция
```hs
-- Подготовим песочницу
import Data.Traversable
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5
-- pre-order: 4 2 1 3 5
instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = (foldr f ini r) -- pre-order
    -- foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
-- sandbox done

-- fold на значении-моноиде, помним такое
fold :: (Foldable t, Monoid m) => t m -> m -- свертка "списка" моноидов
fold = foldr mappend mempty

-- аналогичная функция для одно-параметрических конструкторов: `asum`
-- аппликативный функтор с моноидальной структурой: альтернатив
-- свертка "списка" альтернатив
asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty -- fold для альтернатива (сворачивается контекст, не значения)
-- реализация как для моноида, только оператор другой

-- example

ghci> fmap Just testTree -- получим `t (f a)` где фолдабл это дерево, альтернатив это мэйби, а это число
Branch (Branch (Branch Nil (Just 1) Nil) (Just 2) (Branch Nil (Just 3) Nil)) (Just 4) (Branch Nil (Just 5) Nil)
-- pre-order: 4 2 1 3 5
ghci> asum $ fmap Just testTree
Just 4 -- семантика First, обход пре-ордер

ghci> asum $ fmap (\ _ -> Nothing) testTree
Nothing
```
repl

### 2.2.3 sequenceA_ (fold for Applicative)

Сделаем `fold` для контекста `(Foldable t, Applicative f)`
это будет `sequenceA_`: производство эффектов "списка" (цепочки) аппликативов, с игнорированием значений.
```hs
-- для справки
fold :: (Foldable t, Monoid m) => t m -> m -- свертка "списка" моноидов
fold = foldr mappend mempty
ghci> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- аналог фолда для аппликатива будет:
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())
-- "список" аппликативов связывается в цепочку (пайплайн) аппликативов на операторе "насрать на значение, сделай эффекты".
-- Для каждого конкретного инстанса (класса) надо смотреть семантику аппликатива (эффекта).

ghci> :t (*>) -- вместо mappend, правая `applied over', для попадания в сигнатуру foldr
(*>) :: Applicative f => f a -> f b -> f b
-- выполняет эффекты как и полноценный ап, но игнорирует левое значение

ghci> :t (<*>) -- не годится для foldr
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- т.е. вызов сиквенс для фолдабл аппликатива развернется в такое:
sequenceA_ [ap1, ap2, ap3] = ap1 *> (ap2 *> (ap3 *> pure ())) -- последовательность эффектов: слева-направо

(<*>) :: f (a -> b) -> f a -> f b -- оригинальная сигнатура `applied over`
(*>) :: f a -> f b -> f b -- похоже на flip const; значение берем из второго параметра, эффекты протягиваем из первого и второго
u (*>) v = (pure $ flip const) <*> u <*> v

-- examples

ghci> sequenceA_ (fmap Just testTree)
Just ()

sequenceA_ $ fmap (putStrLn . show) testTree
ghci> sequenceA_ $ fmap print testTree
4
2
1
3
5
ghci> toList testTree
[4,2,1,3,5]

ghci> sequenceA_ $ fmap Left testTree 
Left 4 -- значения игнорит, прокидывает эффекты. Семантика аппликатива Either дает нам первый встреченный Left

ghci> sequenceA_ [("AB", 1), ("CD", 2)]
("ABCD",()) -- эффект: "запись в лог", первые компоненты каждой пары работают

ghci> sequenceA_ [[1,2,3], [4,5]]
[(),(), (),(), (),()] -- три раза по два, семантика эффектов аппликатива списка
```
repl

### 2.2.4 test

```hs
{--
Предположим для двоичного дерева

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

реализован представитель класса типов `Foldable`
обеспечивающий стратегию обхода pre-order traversal.
(также функтор)

Какую строку вернет следующий вызов

GHCi> tree = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
GHCi> fst $ sequenceA_ $ (\x -> (show x,x)) <$> tree
--}

-- solution

"21435"

-- почему?

ghci> toList tree -- порядок обхода дерева
[2,1,4,3,5] -- pre-order

ghci> sequenceA_ $ (\x -> (show x,x)) <$> tree -- в значения дерева записали пары (строка, значение)
("21435",()) -- после прохода сиквенсом-для-эффектов, получим лог: конкатенацию строк в порядке обхода
-- ибо первые значения пар это эффект лога, см. семантику аппликатива для пары
```
test

### 2.2.5 traverse_ (foldMap for Applicative)

foldMap для Applicative называется `traverse_`
```hs
-- memory refresher, как из фолд получается секвенсА
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

-- foldMap моноида превращается в traverse_ аппликатива, по аналогии с fold -> sequenceA_

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием
foldMap f = foldr (mappend . f) mempty

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
-- что здесь происходит, семантика: правой сверткой мы "итерируем" "список" аппликативов,
-- для каждого значения в аппликативе применяем данную функцию,
-- завернутую в "правый ап", что дает нам выполнение эффектов и отбрасывание значений

-- examples

ghci> traverse_ (\ x -> (show x, x*x)) [1, 2]
("12",()) -- согласно поведению аппликатива пары, где первое значение моноид строки,
-- имеем накопленный лог и игнор значений из вторых элементов пар

-- как игнорируются значения в "правом apply over": spoiler: totally
ghci> traverse_ (\ x -> (show x, print x)) [1, 2]
("12",())
ghci> traverse_ (\ x -> (show x, undefined)) [1, 2]
("12",())

-- семантика Either как аппликатива
ghci> traverse_ (\ x -> if x > 0 then Right x else Left x) [1, 2]
Right ()
ghci> traverse_ (\ x -> if x > 0 then Right x else Left x) [1, (-2)]
Left (-2)
ghci> traverse_ (\ x -> if x > 0 then Right x else Left x) [(-1), 2]
Left (-1)
ghci> traverse_ (\ x -> if x > 0 then Right x else Left x) [(-1), (-2)]
Left (-1)

-- фолдабл: список, аппликатив: список
ghci> traverse_ (\ x -> [x+10, x+20]) [1,2]
[(),(), (),()]
ghci> traverse_ (\ x -> [x+10, x+20]) [1, 2, 3]
[(),(), (),(), (),(), (),()] -- 2^3 = 8, 2*2*2, три раза мы "умножили" внутренний список

traverse_ f [1, 2, 3] = f 1 *> (f 2 *> (f 3 *> pure () ))
f x = [x+10, x+20] -- два элемента в списке
= [11, 21] *> ([12, 22] *> ([13, 23] *> [()]))
[13, 23] *> [()] -- даст 2 элемента
[12, 22] *> [_, _] -- даст 4 элемента
[11, 21] *> [_, _, _, _] -- даст 8 элементов
-- согласно семантике аппликатива-списка

ghci> traverse_ (\ x -> [x+10, x+20, x+30]) [1, 2]
[(),(),(),(),(),(),(),(),()] -- 3^2 = 9
```
repl

### 2.2.6 sequenceA2list, необходимость тайпкласса Traversable

Хотим теперь использовать полную версию `apply over <*>`, чтобы не игнорировать значения.
Как можно это сделать для Foldable Applicative?
Собирать значения в список.
Но превращение любого фолдабл в список это не лучшее решение, мы хотим клонировать структуру ...
```hs
-- уже есть у нас сиквенс, работает с эффектами
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

-- допустим, мы хотим, чтобы
ghci> sequenceA_ [("ab", 1), ("cd", 2)]
("abcd",())
-- возвращала нам значения упакованные в список

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2list = foldr (\ x y -> pure (:) <*> x <*> y) (pure [])

-- список: фолдабл, пара: аппликатив
ghci> sequenceA2list  [("ab", 1), ("cd", 2)]
("abcd",[1,2]) -- полная версия `applied over <*>`, есть и эффекты и значения

ghci> sequenceA2list  [Right 1, Right 2]
Right [1,2]
ghci> sequenceA2list  [Left 1, Right 2]
Left 1

-- вроде ОК? нет, есть проблема
ghci> pairTree = fmap (\x -> (show x, x)) (Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)))
ghci> sequenceA_ pairTree 
("21435",()) -- как и положено, выдали эффекты и юнит
ghci> sequenceA2list pairTree -- что мы тут хотим?
("21435",[2,1,4,3,5]) -- эффекты и список значений? Нет, мы хотим дерево значений?
-- мы не хотим превращать любой фолдабл в список, это не универсально

-- хотим внутри аппликатива результата получить тот-же фолдабл, что и на входе,
-- сохранить структуру фолдабл
sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)

-- увы, фолдабл не позволяет восстановить (клонировать) структуру,
-- в его интерфейсе нет такого метода.
-- Поэтому нам нужен Traversable
```
repl

### 2.2.7 test

```hs
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
traverse2list = undefined

-- solution
-- traverse_ это foldMap для "списка" аппликативов

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

-- alternative

traverse2list f x = traverse f (toList x) -- как-бы читерство, не то, что в лекции показано
-- превращается в:
import Data.Foldable
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list = (. toList) . traverse

-- берём toList
toList :: Foldable t => t a -> [a]
-- берём любую функцию, которая может обработать его результат
(. toList) :: Foldable t => ([a] -> c) -> t a -> c
-- ключевой момент: вот это сечение оператора композиции дает нам:
-- возможность задать первым параметром функцию-обработчик-списка,
-- вторым параметром изначальный фолдабл.
-- Т.е. мы получаем некий адаптер, которому надо скормить нужный обработчик.
-- Можно посмотреть так: композиция дает нам прямой пайплайн: получить список, пройтись по нему.

-- результат traverse - функция ([a] -> f [b]), подходящая на вход в (. toList), 
-- подставляем результат traverse на вход в (. toList)  - композиция
(.toList) .traverse :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]

-- О выводе (обратный ход)
-- берём очевидное решение
traverse2list f x = traverse f (toList x)
-- выражаем через x
traverse2list f x = (traverse f) (toList x)
traverse2list f x = (traverse f . toList) x
-- η  - преобразование 1
traverse2list f = traverse f . toList
-- выражаем через f
traverse2list f = (. toList) (traverse f)  
traverse2list f = ((. toList).traverse) f  
-- η  - преобразование 2
traverse2list = (. toList).traverse

```
test

### 2.2.8 type-class Traversable

Траверсабл не только производит эффекты (аппликативные), как фолдабл,
но и клонирует структуру, по которой итерируется
```hs
-- расширение функтора и фолдабла
class (Functor t, Foldable t) => Traversable t where

    -- коммутация "контейнеров", вытащить аппликатив наружу
    sequenceA :: Applicative f => t (f a) -> f (t a) -- "список" аппликативов преобразовать в аппликатив "списка"
    sequenceA = traverse id

    -- есть функция с эффектами и "список", проехавшись по контейнеру этой функцией мы соберем эффекты и "список" результатов
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    -- "список" а преобразовать в аппликатив "списка" бе, используя "лифт" (a -> f b)
    traverse = sequenceA . fmap

-- Minimal complete definition
-- traverse | sequenceA
```
repl

### 2.2.9 instance Traversable Maybe

Представители (инстансы) траверсабл, реализация для: мейби.

Реализация траверсабл повторяет реализацию функтора (fmap), с добавлением прослойки типа аппликатив
```hs
class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a) -- "список" аппликативов преобразовать в аппликатив "списка"
    sequenceA = traverse id
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = sequenceA . fmap

-- Functor t, Foldable t -- это мейби
instance Traversable Maybe where
    -- завернуть наш фолдабл функтор Maybe в некий аппликативный функтор, заворачивание в аппликатив через pure
    traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing -- функция бесполезна для "ничего"
    traverse g (Just x) = (pure Just) <*> (g x)
    -- pure Just поднимает конструктор в аппликатив, после чего произойдет `apply over` (g x)

-- examples

ghci> traverse (\ x -> [x, x+2, x^2]) (Just 5)
[Just 5, Just 7, Just 25] -- аппликатив это список
-- выполнены эффекты, список из трех элементов;
-- воспроизведена структура фолдабл функтора мэйби: результаты завернуты в Just

ghci> traverse (\ x -> [x, x+2, x^2]) (Nothing)
[Nothing] -- почему не три? Согласно семантики траверсабл "ничего": аппликатив ничего
-- эффект произведен: список; структура фолдабла воспроизведена

ghci> sequenceA (Just ("foo", 42))
("foo",Just 42) -- фолдабл (мейби) аппликатива (пары) преобразовать в аппликатив (пару) фолдабла (мейби)

-- обратите внимание, реализация траверсабл очень похожа на реализацию функтора
-- так оно и понятно: семантика аналогичная, протащить функцию в контекст
-- только для траверсабла добавляется контекст "вычисления с эффектами", аппликативный функтор дает нам
-- вычисление с эффектами. Для поддержки этого нам и нужно сделать траверсабл, ибо просто функтора недостаточно

class Functor f where
    fmap :: (a -> b) -> f a -> f b -- fmap or <$>
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just $ g x

instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse g (Just x) = (pure Just) <*> (g x)

-- траверсабл позволяет нам обобщить функтор (вычисление в контексте) до цепочки вычислений с эффектами
-- это наблюдение позволяет нам, имея реализацию функтора для (конструктора) типа, написать траверсабл механически
```
repl

### 2.2.10 instance Traversable `((,) s)`

траверсабл для пары
```hs
-- посмотрим на функтор для пары
class Functor f where
    fmap :: (a -> b) -> f a -> f b
instance Functor ((,) s) where
    fmap g (x, y) = (x, g y)
    fmap g (x, y) = (,) x (g y) -- применение конструктора к данным

instance Traversable ((,) s) where
    traverse :: Applicative f => (a -> f b) -> (s, a) -> f (s, b)
    traverse g (x, y) = (pure ((,) x)) <*> (g y) -- аналогично: поднять конструктор пары в аппликатив, ап его на результат (g y)
    traverse g (x, y) = ((,) x) <$> (g y) -- (см. fmap) согласно закону левого pure для аппликатива

-- examples

ghci> sequenceA ("foo", Just 42)
Just ("foo", 42)

ghci> sequenceA $ sequenceA ("foo", Just 42)
("foo", Just 42)
```
repl

### 2.2.11 test

```hs
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

-- solution
-- мы уже сделали его фолдабл
-- уже была реализация Applicative для Triple

-- data Triple a = Tr a a a  deriving (Eq,Show)
instance Traversable Triple where
    -- traverse :: (Applicative f)=> (a -> f b) -> (Triple a) -> f (Triple b)
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z) -- см. функтор, плюс протаскивание конструктора в аппликатив
{--
instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure :: a -> Triple a
    pure x = Tr x x x
    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (Tr fx fy fz) <*> (Tr x y z) = Tr (fx x) (fy y) (fz z)
instance Foldable Triple where
    foldMap f (Tr x y z) = (f x) `mappend` ((f y) `mappend` (f z))
--}

-- alternative

instance Traversable Triple where
    -- sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA (Tr x y z) = Tr <$> x <*> y <*> z -- затаскивание конструктора внутрь

```
test

### 2.2.12 test

```hs
{--
Сделайте тип данных `Result`

data Result a = Ok a | Error String deriving (Eq, Show)

представителем класса типов `Traversable` (и всех других необходимых классов типов).

GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
[Ok 7,Ok 3]
GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
[Error "!!!"]
--}

-- solution

-- data Result a = Ok a | Error String deriving (Eq, Show)
instance Functor Result where
    -- fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Error s) = Error s
    fmap f (Ok x) = Ok $ f x

-- Applicative вообще не надо было реализовывать
instance Applicative Result where
    -- pure :: a -> Result a
    pure = Ok
    -- (<*>) :: Result (a -> b) -> Result a -> Result b
    (Ok f) <*> (Ok x) = Ok $ f x
    (Error s) <*> _ = Error s -- ошибки можно склеивать, не надо?
    _ <*> (Error s) = Error s

instance Foldable Result where
    -- foldMap :: Monoid m => (a -> m) -> Result a -> m
    foldMap _ (Error s) = mempty
    foldMap f (Ok x) = f x

instance Traversable Result where
    -- traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse f (Error s) = pure $ Error s
    traverse f (Ok x) = Ok <$> f x

-- alternative

instance Functor Result where
  fmap f (Ok x)    = Ok (f x)
  fmap _ (Error s) = Error s

instance Foldable Result where
  foldMap f (Ok x)    = f x
  foldMap _ (Error _) = mempty

instance Traversable Result where
  sequenceA (Ok x)    = Ok <$> x
  sequenceA (Error s) = pure (Error s)

```
test

### 2.2.13 instance Traversable `[]`

Траверсабл для списка, протаскивание аппликатива в список.
Если аппликатив тоже список, имеем головоломку
```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
instance Functor [] where
    fmap _ [] = []
    fmap g (x:xs) = (:) (g x) (fmap g xs) -- префиксная запись рекурсивной реализации map

instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse _ [] = pure []
    traverse g (x:xs) = (:) <$> (g x) <*> (traverse g xs)

-- examples

ghci> traverse (\x -> (show x, x^2)) [1,2,3]
("123",[1,4,9]) -- и эффекты (накопления лога в паре) и структура (списка) и значения -- все есть

ghci> traverse (\x -> [x+10, x+20]) [1,2,3] -- 2^3 = 8, эффект множественных вычислений в аппликативе списка
[
    [11,12,13],[11,12,23],
    [11,22,13],[11,22,23],
    [21,12,13],[21,12,23],
    [21,22,13],[21,22,23]
    ]
-- для трех элементов входа будет построена такая выражения
(:) <$> [11, 21] <*> ((:) <$> [12, 22] <*> ((:) <$> [13,23] <*> pure []))
-- после чего аппликатив сделает "каждый с каждым"; эти числа заполняют места 1 2 3 в триадах,
-- причем порядок в выражении указывает на их порядок в триадах
[13,23] <*> pure [] -- два результата (два варианта последних элементов триад)
[12, 22] <*> ((:) <$> [13,23] <*> pure []) -- два на два = четыре триады (два варианта предпоследних элементов, для двух вариантов последних)
[11, 21] <*> ((:) <$> [12, 22] <*> ((:) <$> [13,23] <*> pure [])) -- два на четыре = восемь элементов:
-- предыдущие четыре триады два раза с этими двумя значениями на первых местах триады

ghci> sequenceA [[11,21], [12,22], [13,23]] -- traverse id
[[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
-- наружная структура уходит внутрь (три элмента), аппликатив списка дает внутрениие циклы "каждый с каждым"

-- список операций протянуть через список значений, количество операций дает количество вариантов для каждого числа из входа [1,2,3]
ghci> traverse (\x -> [x^2, x+10]) [1,2,3] -- каждое число будет ^2, +10, т.е. даст два варианта (две ветки вычислений других чисел)
[
    [1,4,9],
    [1,4,13],
    [1,12,9],
    [1,12,13],
    [11,4,9],
    [11,4,13],
    [11,12,9],
    [11,12,13]
    ]
-- трех-элементный контейнер восемью способами
-- [x, y, z]: z = 9, 13 два варианта последнего числа (3 ^2, +10)
-- y: 4, 12 два варианта второго числа (2 ^2, +10)
-- x: 1, 11 два варианта первого числа (1 ^2, +10)

-- это была демонстрация семантики "каждый с каждым". Что насчет семантики "зип"?

ghci> sequenceA (map ZipList [[11,21], [12,22], [13,23]]) -- два элемента, внутри которых будут результаты зипа трех списков
ZipList {getZipList = [[11,12,13],[21,22,23]]} -- список первых элементов, список вторых элементов
-- идентично
ghci> traverse ZipList [[11,21], [12,22], [13,23]]
ZipList {getZipList = [[11,12,13],[21,22,23]]}
-- внешняя структура ушла внутрь (три элемента), результатов всего два согласно семантике зип:
-- по размеру списков на входе аппликативного зипа

-- traverse ZipList сработал как транспонирование матрицы
    [11,21], 
    [12,22], 
    [13,23]
->
    [11,12,13],
    [21,22,23]

```
repl

### 2.2.14 test

```hs
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

-- solution
-- дерево (как и список) представляет коллекцию. Какую семантику аппликатива для дерева выбрать?

-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)
instance Traversable Tree where
    -- sequenceA :: (Applicative f)=> Tree (f a) -> f (Tree a)
    sequenceA Nil = pure Nil
    sequenceA (Branch l m r) = Branch <$> (sequenceA l) <*> m <*> (sequenceA r)

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldr f ini Nil = ini
    -- foldr f ini (Branch l x r) = f x (foldr f iniR l) where iniR = foldr f ini r -- pre-order
    foldr f ini (Branch l x r) = foldr f (f x iniR) l  where iniR = (foldr f ini r) -- in-order

-- alternative

import           Data.Monoid
instance Functor Tree where
  fmap _ Nil            = Nil
  fmap f (Branch l m r) = Branch (fmap f l) (f m) (fmap f r)

instance Foldable Tree where foldMap = foldMapInt (\ l m r -> l <> m <> r)

instance Traversable Tree where
  sequenceA Nil            = pure Nil
  sequenceA (Branch l m r) = Branch <$> sequenceA l <*> m <*> sequenceA r

foldMapInt::Monoid b=> (b->b->b->b) -> (a->b) -> Tree a -> b
foldMapInt g f = go where
  go Nil            = mempty
  go (Branch l m r) = g (go l) (f m) (go r)

```
test

### 2.2.15 test

```hs
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

-- solution

instance (Traversable a, Traversable b)=> Traversable (a |.| b) where
    -- traverse :: (Applicative f) => (x -> f y) -> (|.|) a b x -> f ((|.|) a b y)
    traverse f (Cmps x) = Cmps <$> traverse (traverse f) x -- протащить через два барьера
{--
instance (Functor f, Functor g)=> Functor (f |.| g) where
    -- fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x
--}

-- alternative

instance (Traversable f, Traversable g)=>Traversable (f |.| g) where
    sequenceA (Cmps x) = Cmps <$> sequenceA (sequenceA <$> x) -- два раза вывернуть наизнанку

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
    sequenceA (Cmps x) = Cmps <$> traverse sequenceA x

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    sequenceA = fmap Cmps . traverse sequenceA . getCmps

```
test

## chapter 2.3, Законы и свойства класса Traversable

https://stepik.org/lesson/31555/step/1?unit=11808

- Законы Traversable: identity
- Законы Traversable: composition
- Гарантии, обеспечиваемые законами Traversable
- Фантомные типы
- Функтор Const
- Реализации методов базовых классов по умолчанию: fmapDefault
- Реализации методов базовых классов по умолчанию: foldmapDefault
- Полное определение класса Traversable

### 2.3.2 traverse identity law

Есть шесть законов для траверсабл, по сути они похожи на законы для функтора,
что не удивительно, учитывая их близкую природу
```hs
-- identity law
-- вспомним, как это было для функтора

class Functor f where
    fmap :: (a -> b) -> f a -> f b -- <$>

fmap id = id -- identity law для функтора, структура не меняется (и значения не меняются)

-- для траверсабла так нельзя,
-- 1: по сигнатуре ф. id сюда не подходит,
-- 2: сам смысл траверса в том, что меняется структура (аппликатив наружу, траверс. внутрь)
-- как быть?

class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

traverse Identity = Identity -- identity law для траверсабл
-- траверс без эффектов не должен менять контекст (не вносит отсебятины)

ghci> :t Identity -- аппликативный функтор
Identity :: a -> Identity a

ghci> :i Identity -- контекст без эффектов
newtype Identity a = Identity {runIdentity :: a} -- Defined in ‘Data.Functor.Identity’

instance Applicative Identity where
    pure = Identity
    (Identity g) <*> (Identity x) = Identity $ g x

ghci> traverse Identity $ "foo"
Identity "foo"

ghci> (traverse Identity $ "foo") == (Identity "foo")
True

-- т.е. при протаскивании стрелки Клейсли (айдентити) через "список", с последующим вытаскиванием
-- аппликатива (эффектов) наружу, мы получим то же самое, что и на входе.
-- Ничего не поменяется, id, при условии протаскивания без-эффектной функции через траверс

```
repl

### 2.3.3 traverse composition law

Закон композиции траверсов (идентичность траверсу композиции стрелок Клейсли)
```hs
-- Как это было для функтора
class Functor f where
    fmap :: (a -> b) -> f a -> f b -- <$>

fmap (g1 . g2) = (fmap g1) . (fmap g2) -- закон композиции для функтора

-- А теперь для траверса

class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- закон композиции для траверс
traverse $ Compose . (fmap g2) . g1 = Compose . (fmap $ traverse g2) . (traverse g1)
-- траверс композиции эквивалентен композиции траверсов
-- с учетом выворачивания двух контекстов: аппликатива и траверсабла
-- но почему g2, g1 а не g1, g2 ?

ghci> :i Compose
newtype Compose f g a = Compose {getCompose :: f (g a)} -- Defined in ‘Data.Functor.Compose’
infixr 9 `Compose` -- право-ассоциативный

ghci> :t Compose
    f (g a) -> Compose f g a

ghci> :t Compose (Just $ Right "foo")
Compose (Just $ Right "foo") :: Compose Maybe (Either a) String
ghci> :t Just $ Right "foo"
Just $ Right "foo" :: Maybe (Either a String)

-- разберем происходящее: композиция стрелок Клейсли (в ап.функтор) использованная в одном траверс,
-- эквивалентна двум последовательным траверсам (композиции) для каждой стрелки по отдельности
traverse $ Compose . (fmap g2) . g1 = Compose . (fmap $ traverse g2) . (traverse g1)
traverse (Compose . fmap g2 . g1) ta = Compose (fmap (traverse g2) (traverse g1 ta))

ta :: t a -- traversable
g1 :: Applicative f1 => a -> f1 b
g2 :: Applicative f2 => b -> f2 c
(traverse g1 ta) :: f1 (t b) -- вот тут видно, что второй траверс сюда не влезает, нужен fmap для поднятия внутрь f1

-- example для правой части закона композиции
ghci> fmap (traverse Right) (traverse Just "foo")
Just (Right "foo")
-- fmap протащил Right внутрь Just

ghci> (traverse Just "foo")
Just "foo"
ghci> (traverse Right) (traverse Just "foo")
Right (Just "foo")

ghci> :t fmap (traverse Right) (traverse Just "foo")
  :: Maybe (Either a [Char])
ghci> :t Compose (fmap (traverse Right) (traverse Just "foo"))
  :: Compose Maybe (Either a) [Char]

-- левая часть
ghci> traverse (Compose . fmap Right . Just) "foo"
Compose (Just (Right "foo")) -- результат такой же, Just вылез наружу
ghci> :t traverse (Compose . fmap Right . Just) "foo"
  :: Compose Maybe (Either a) [Char]

ghci> :t  (Compose . fmap Right . Just)
     :: a1 -> Compose Maybe (Either a2) a1

ghci> :t  (fmap Right . Just)
     :: a1 -> Maybe (Either a2 a1)
-- a1 -> Maybe (Either a2 a1)
-- видно, что это стрелка Клейсли, подходит для передачи в traverse первым аргументом
```
repl

### 2.3.4 Traversable laws, остальные (не столь важные)

Выполнение законов дает некоторые гарантии `traverse`
- посещает все узлы структуры
- каждый узел посещается ровно 1 раз
- реализация `pure` для аппликатива: тривиальна (без эффектов и вычислений)
- не меняет структуру контейнера (клонирует контекст), хотя может выдать "пустой" контейнер,
если аппликативный функтор реализует такой эффект (WHA?)

`traverse` это по сути `fmap` но с эффектами, обеспечиваемыми аппликативными функторами.

Остальные 4 закона для `Traversable`
```hs
-- law #3, traverse naturality, free theorem
-- если:
t :: (Applicative f, Applicative g) => f a -> g a -- аппликативный гомоморфизм, со свойствами:
t (pure x) = pure x
t (x <*> y) = (t x) <*> (t y)
-- то:
t . traverse g = traverse (t . g) -- вынос t из траверса ничего не меняет

-- оставшиеся три закона повторяют предыдущие законы, но для функции sequenceA
sequenceA . fmap Identity = Identity                            -- identity
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA -- composition
t . sequenceA = sequenceA . fmap t                              -- naturality
-- эти законы фиксируют реализацию sequenceA через traverse (и наоборот),
-- при правильной реализации, законы выполняются автоматически.
```
repl

```hs
https://stepik.org/lesson/31555/step/5?unit=11808
TODO
{--
В предположении что обе части закона `composition` для `sequenceA`

sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA

имеют тип

(Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)

укажите тип подвыражения `fmap sequenceA` в правой части. Контекст указывать не надо.
--}

-- solution

```
test

```hs
https://stepik.org/lesson/31555/step/6?unit=11808
TODO
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

-- solution

```
test

### 2.3.7 фантомные типы

> A phantom type is a parameterised type whose parameters do not all appear on the right-hand side of its definitio
https://wiki.haskell.org/Phantom_type

Враппер дизайн-тайм для создания разных типов на базе одного.
Чтобы тайп-чекер ловил нас за руку при попытки сложить килогрыммы и градусы.

Мотивация: допустим мы работаем с температурой, выражаемой через число.
чтобы не смешивать температуру с килограммами, выразим темп. через `newtype`.
Но темп. у нас есть по фаренгейту и по цельсию ...
```hs
-- одно-параметрический конструктор типа, конструктор данных не-полиморфный
newtype Temperature a = Temperature Double -- фантомный тип
    deriving (Num, Show) -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving
-- `a` это фантомный тип, в правой части не появляется нигде

data Celsius -- типы данных без контструкторов данных, не населенные типы (пустые множества, void)
data Farenheit

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Farenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)
```
repl

```hs
https://stepik.org/lesson/31555/step/8?unit=11808
TODO
{--
Расширьте интерфейс для работы с температурами из предыдущего видео
Кельвинами
и реализуйте функцию 

k2c :: Temperature Kelvin -> Temperature Celsius

обеспечивающую следующее поведение

GHCi> k2c 0
Temperature (-273.15)
GHCi> k2c 0 == Temperature (-273.15)
True
GHCi> k2c 273.15
Temperature 0.0
--}
newtype Temperature a = Temperature Double
  deriving (Num,Show)

data Celsius
data Fahrenheit 

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c = undefined

-- solution

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Temperature a = Temperature Double
  deriving (Num,Show,Eq)

data Celsius
data Fahrenheit 
data Kelvin 

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Double -> Temperature Celsius
k2c k = Temperature (k - 273.15)

```
test

### 2.3.9 фантомный тип `newtype Const c a`

Ранее мы видели полезные примитивы: `Identity`, `Compose`.
Посмотрим на еще один: `Const`
```hs
newtype Const c a = Const { getConst :: c } deriving (Eq, Show)
-- фантомный тип (a), игнорирует второй параметр конструктора типа
-- семантика похожа на семантику пары: лог есть а значения нет.

ghci> Const 'z'
Const {getConst = 'z'}
ghci> :t Const 'z'
    :: Const Char a
ghci> :k Const
Const :: * -> k -> *

instance Functor (Const c) where -- связали первый параметр конструктора типов, обеспечили нужный kind
    fmap :: (a -> b) -> Const c a -> Const c b -- сигнатура говорит, что меняется только фантомный тип
    fmap _ (Const v) = Const v -- т.е. в функторе мы просто перевешиваем метку типа на константном значении

ghci> fmap (^2) (Const 'z')
    Const {getConst = 'z'} -- реализация игнорит функцию, только меняет тип ...
ghci> :t fmap (^2) (Const 'z')
    :: Num b => Const Char b -- да, появилось требование Num b
ghci> :t Const 'z' -- у оригинала такого не было:
    :: forall {k} {a :: k}. Const Char a

ghci> :t fmap length (Const 'z') -- еще интереснее: теперь второй параметр нужен Int
fmap length (Const 'z') :: Const Char Int
-- меняем фантомный тип, по факту.

instance Foldable (Const c) where
    foldMap :: (Monoid m) => (a -> m) -> Const c a -> m
    foldMap _ _ = mempty -- поскольку в "контейнере" нет значений `a` (только "лог"), то это поведение естественно

instance (Monoid c) => Applicative (Const c) where
    pure :: a -> Const c a
    pure _ = Const mempty
    (<*>) :: Const c (a -> b) -> Const c a -> Const c b -- a, b это фантомы, их нет
    (<*>) (Const f) (Const v) = Const (mappend f v) -- семантика пары: (лог, _)

ghci> pure 'z' :: Const [a] Char -- демо, аппликатив, укажем тип: где первый параметр это моноид (список)
Const {getConst = []} -- переданный 'z' это значение фантомного типа, исчезло

ghci> Const "ab" <*> Const "cd" -- вторые параметры фантомные, первые сконкатенированы в операторе `applied over'
Const {getConst = "abcd"}

instance Traversable (Const c) where
    traverse :: (Applicative f) => (a -> f b) -> Const c a -> f (Const c b)
    traverse _ (Const v) = pure (Const v) -- подняли в аппликатив и поменяли метку типа с `a` на `b`
    -- по аналогии с функтором
```
repl

### 2.3.10 `fmapDefault` для вывода Functor из Traversable

В Хаскел можно провернуть интересный трюк:
реализовать, скажем, `Traversable` для некоего типа, после чего для этого типа,
реализации `Functor` и `Foldable` появятся автомагически.
Посмотрим, как это возможно (спойлер: существует функция `fmapDefault`, реализующая `fmap`).
А еще это возможно из-за ад-хок полиморфизма: в наличии инстансы с реализацией нужных методов.
```hs
class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a) -- "список" аппликативов преобразовать в аппликатив "списка"
    sequenceA = traverse id
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = sequenceA . fmap

data Result a = Error | Ok a deriving (Eq, Show, Functor, Foldable) -- sum-type, Maybe isomorph

-- instance (Functor Result, Foldable Result) => Traversable Result where
instance Traversable Result where
    traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
    traverse _ Error = pure Error
    traverse f (Ok x) = (pure Ok) <*> (f x)
    -- traverse f (Ok x) = Ok <$> f x

ghci> traverse (\x -> [x+10, x+20]) (Ok 5)
[Ok 15,Ok 25]

-- почему это возможно: fmapDefault

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b -- fmap
fmapDefault f = runIdentity . traverse (Identity . f) -- по сути, подняли ф в контекст траверсабла,
-- мы уже видели, что траверсабл и функтор это близнецы-братья
-- траверс должен был бы дать эффекты от ап.функтора, но айдентити не содержит эффектов, поэтому тут эффектов не будет,
-- эквивалентно поведению функтора

instance Functor Result where fmap = fmapDefault

ghci> :t fmap -- для справки
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> fmapDefault (^2) (Ok 5)
Ok 25
ghci> fmap (^2) (Ok 5)
Ok 25

-- фолдабл делается аналогично, см.
ghci> :t foldMapDefault 
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m

```
repl

### 2.3.11 `foldMapDefault` для вывода Foldable из Traversable

Продолжим предыдущую тему. Как, имея траверсабл, сделать фолдабл?
```hs
-- для справки, Foldable реализуется через фолдмэп:
ghci> :t foldMap
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- фолдабл делается через универсальную функцию, реализующую фолдМэп для траверсабла
ghci> :t foldMapDefault 
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m

foldMapDefault f = getConst . traverse (Const . f) -- траверс требует аппликатив и использует его для построения цепочки
-- поднимем ф в аппликатив, используем его эффекты как сумматор свертки

-- как мы помним, аппликатив Конст сделан так:
-- пюре дает пустой моноид; апплайд-овер дает конкатенацию моноидов, что и требуется
instance (Monoid c) => Applicative (Const c) where
    pure :: a -> Const c a
    pure _ = Const mempty
    <*> :: Const c (a -> b) -> Const c a -> Const c b -- a, b это фантомы, их нет
    (<*>) (Const f) (Const v) = Const (mappend f v) -- семантика пары: (лог, _)

instance Foldable Result where foldMap = foldMapDefault

ghci> foldr (+) 37 (Ok 5)
42
```
repl

```hs
https://stepik.org/lesson/31555/step/12?unit=11808
TODO
{--
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

представителем класса типов `Traversable` таким образом, 
чтобы обеспечить для `foldMapDefault` порядок обхода «postorder traversal»:

GHCi> testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
GHCi> foldMapDefault (\x -> [x]) testTree
[1,3,2,5,4]
--}
import Data.Traversable (foldMapDefault)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA = undefined

-- solution
-- надо либо traverse определять, либо sequenceA и fmap

```
test

### 2.3.13 `sequence`, `mapM` в Traversable для монад

Есть еще две функции класса траверсабл: sequence, mapM
```hs
class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a) -- "список" аппликативов преобразовать в аппликатив "списка"
    sequenceA = traverse id
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = sequenceA . fmap

-- вот они: видно, что жопа та же, только для контекста монады
-- всякая монада является аппликативным функтором,
-- не всякий аппликативный функтор можно расширить до монады

    sequence :: (Monad m) => t (m a) -> m (t a)
    sequence = sequenceA

    mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
    mapM = traverse

-- эти функции были добавлены в Хаскел задолго до появления аппликатива,
-- были сделаны свободными (вне класса) и требовали списка а не траверсабла
```
repl

## chapter 2.4, Связь классов Monad и Applicative

https://stepik.org/lesson/28881/step/1?unit=9913

- Направление вычислений в цепочке
- Цепочки вычислений и порядок эффектов: монады
- Цепочки вычислений и порядок эффектов: аппликативные функторы
- Applicative и Monad: отличия в работе с эффектами
- Applicative как базовый класс для Monad
- Монады: реализация методов Applicative по умолчанию
- Монады: реализация метода Functor по умолчанию
- Статус функции `fail`: настоящее и будущее

### 2.4.2 пайплайн вычислений (`$, <$>, &, <&>`)

На текущий момент мы знаем два способа делать вычисления с эффектами:
монады (из первой части курса) и аппликативные функторы.
Какие отношения связывают монады и ап.функторы?

Сами по себе (изолированно) вычисления в монаде или в ап.функторе не очень интересны.
Интересно начинается, когда работает вычислительный пайплайн, комбинация, цепочка вычислений.

Посмотрим, как устроены цепочки вычислений, в монадах и ап.функторах
```hs
-- Операторы "аппликации":

-- функция, затем аргумент:
($)     ::                      (a -> b) ->   a ->   b -- infixr 0 $
(<$>)   :: Functor     f  =>    (a -> b) -> f a -> f b -- infixl 4 <$>, fmap
(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b -- infixl 4 <*>
(=<<)   :: Monad       m  =>  (a -> m b) -> m a -> m b -- infixr 1 =<<
-- аргумент, затем функция (через flip, за одним исключением - Applicative):
(&)     ::                      a ->   (a -> b) ->   b -- infixl 1 &    -- Data.Function
(<&>)   :: Functor     f  =>  f a ->   (a -> b) -> f b -- infixl 1 <&>  -- Control.Lens.Operators
(<**>)  :: Applicative f  =>  f a -> f (a -> b) -> f b -- infixl 4 <**> -- Control.Applicative
(>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b -- infixl 1 >>=

-- ($): правая ассоциативность с низким приоритетом позволяет убирать скобки, что удобно

-- examples (смотрите на направление пайплайнов)

ghci> (+1) $ (^2) $ 3 -- 3^2 + 1
10 -- +1 (^2 3) -- запись в стиле "вызов функций

ghci> 3 & (^2) & (+1) -- более привычный вид пайплайна, слева-направо
10 -- (3 ^2) +1

ghci> (+1) <$> (^2) <$> [1, 2, 3] -- направление и читабельность аналогично `$`
[2,5,10] -- аппликация "в контейнере", функция поднимается в контекст, fmap;
-- эффектов нет, структура "контейнера" не меняется;
-- но ведь это лево-ассоц. оператор? Да, под капотом:
-- `^2` это функтор стрелочного типа, поэтому `(+1) <$> (^2)` это композиция функций `+1 . ^2`
-- "плюс-один после в-квадрат".

ghci> [1,2,3] <&> (^2) <&> (+1) -- аналогично `&`, но в контексте; `flip fmap`
[2,5,10]

```
repl

### 2.4.3 пайплайн вычислений в монаде (`>>=, =<<`)

Продолжим рассматривать пайплайны вычислений, теперь для монад.

Как мы видели, без эффектов пайплайн простой.
Слева-направо, справа-налево: как удобно так и делай.

Когда появляются эффекты (аппликативный функтор, монада), к порядку вычислений в пайплайне
добавляется порядок выполнения эффектов (и накопления эффектов).
Например: IO, лог (пара), списки (внешний список задает внешний цикл), ...

Как реализован порядок пайплайна для монад (спойлер: без особых сюрпризов, но надо следить за ассоциативностью накопления лога)
```hs
-- монада Writer (pair), ду-нотация
ghci> do {x <- ("b", 2); y <- ("c", 3); return (x, y)}
("bc",(2,3))
-- порядок слева-направо, на выходе (лог, значение-пара)

-- тот же пайплайн (слева-направо) но в низкоуровневой нотации с bind
-- (>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b -- infixl 1 >>=
ghci> ("b", 2) >>= (\x -> ("c", 3) >>= (\y -> return (x, y)))
("bc",(2,3))

-- иллюстрация пайплайна справа-налево, флипнутый bind
-- (=<<)   :: Monad       m  =>  (a -> m b) -> m a -> m b -- infixr 1 =<<
ghci> (\x -> (\y -> return (x, y)) =<< ("c", 3)) =<< ("b", 2)
("bc",(2,3))
```
repl

### 2.4.4 пайплайн вычислений в аппликативе (`<*>, <**>`)

На сладкое: порядок вычислений и эффектов в пайплайне аппликативного функтора.
Это тот особый случай, когда обратный порядок сделан не через `flip`.
Что приводит к разным порядкам вычислений и эффектов в пайплайне а.ф. через `<**>`
```hs
(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b -- infixl 4 <*>
(<**>)  :: Applicative f  =>  f a -> f (a -> b) -> f b -- infixl 4 <**> -- Control.Applicative

-- запишем предыдущий пример (пайплайн слева-направо в монаде)
ghci> do {x <- ("b", 2); y <- ("c", 3); return (x, y)}
("bc",(2,3))

-- как вычисления в аппликативном функторе, слева-направо
ghci> (pure (,) <*> ("b", 2)) <*> ("c", 3)
("bc",(2,3))
-- конструктор пары поднять в а.ф., апплай овер пара1, апплай овер пара2
-- ведь конструктор пары требует двух аргументов
-- реализация а.ф. для пары приводит к ожидаемому итогу.
-- порядок вычислений и порядок эффектов тоже ожидаемый, слева-направо.

-- посмотрим на вариант пайплайна справа-налево
ghci> ("c", 3) <**> (("b", 2) <**> pure (,))
("cb",(2,3)) -- лог перевернутый, значения в правильном порядке
-- несложно догадаться почему: при правильном порядке сборки результата,
-- порядок накопления лога по прежнему слева-направо.
-- ибо реализация этого оператора не через `flip`!
-- Сложнее понять "зачем?": вероятно, затем, что "есть возможность". А вдруг понадобится.
-- для монады и функтора такой возможности нет.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- разберем что происходит, для наглядности перепишем немного
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
xs <**> fs = pure (&) <*> xs <*> fs
-- эффекты по прежнему слева-направо, тогда как вычисления,
-- поднятые в аппликатив, перевернуты, благодаря оператору `&`

-- experiment, сделаем через простой флип
(<***>) :: Applicative f => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)
ghci> ("c", 3) <***> (("b", 2) <***> pure (,))
("bc",(2,3)) -- справа-налево, как (вроде) и должно быть

-- если без эффектов, то разворот цепочки бывает удобнее для чтения
ghci> (+) <*> (*3) $ 2 -- цепочка вычислений (2 +) (2 *3)
8
ghci> (*3) <**> (+) $ 2 -- цепочка развернулась: (2 *3) (+ 2)
8
```
repl

Пример, зачем это может быть нужно (спойлер: сделать DSL на минималках)
```hs
import Text.ParserCombinators.Parsec
import Control.Applicative ((<**>))

expr :: Parser Integer
expr = natural <**> operator <*> natural <* eof

natural :: Parser Integer
natural = read <$> many1 digit

operator :: Parser (Integer -> Integer -> Integer)
operator = choice
  [ (+) <$ char '+'
  , (-) <$ char '-'
  , (*) <$ char '*'
  ]

ghci> parseTest expr "2*2"
4
ghci> parseTest expr "3-2"
1
ghci> parseTest expr "1+2"
3
```
repl


### 2.4.5 отличия аппликатива от монады (`<*>, >>=`)

Посмотрели на пайплайны в монадах и в аппликативах.
Они похожи в том смысле, что позволяют делать пайплайны с эффектами.
А в чем разница?

В аппликативе структура вычислений более жестко фиксирована.
В монаде дозволена большая гибкость при вычислениях, можно влиять на структуру контекста (на эффекты).

> Монада: значение предыдущего вычисления может влиять на структуру следующих вычислений
```hs
-- приведем пример

-- пайплайн в аппликативе списка
ghci> (,) <$> [1..3] <*> [1..3] -- построение пар, каждый с каждым
[
    (1,1),(1,2),(1,3),
    (2,1),(2,2),(2,3),
    (3,1),(3,2),(3,3)]
ghci> (,) <$> [1 .. 3] <*> ['a' .. 'c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

-- пайплайн в монаде списка, результат идентичен
ghci> do { a <- [1 .. 3]; b <- ['a' .. 'c']; return (a, b) }
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

ghci> do { a <- [1 .. 3]; b <- [1 .. 3]; return (a, b) }
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-- смотрите: вложенный цикл мы поменяли, он начинается со значения взятого из внешнего цикла
ghci> do { a <- [1 .. 3]; b <- [a .. 3]; return (a, b) }
[
    (1,1),(1,2),(1,3),
    (2,2),(2,3),
    (3,3)]
-- Монада: значение предыдущего вычисления может влиять на структуру следующих вычислений

-- посмотрите на сигнатуры операторов, для монады функция выглядит иначе,
-- во время ее выполнения создается контекст,
-- как итог, пайплайн связывает как вычисления, так и измениня контекста,
-- что не работает для аппликатива
(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b -- infixl 4 <*>
(>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b -- infixl 1 >>=
-- Согласно сигнатуре, аппликатив не может менять структуру контекста в процессе выполнения функции.

-- посмотрим другой пример: возведение в квадрат (только положительных чисел)
-- в монаде
funM mv = mv >>= (\v -> if v > 0 then return (v ^ 2) else fail $ "got negative value: " ++ show v)
-- в аппликативе
funA mv = pure (^2) <*> mv -- нет возможности сделать подобное

ghci> funM $ Just 3
Just 9
ghci> funM $ Just (-3)
Nothing

-- experiment
funA :: (Applicative f, Num a, Ord a) => f a -> f a
funA mv = pure (\x -> if x > 0 then x^2 else undefined) <*> mv
--  `else undefined`: мы не знаем, что тут написать, у аппликатива нет `fail` есть только pure
ghci> :t fail
fail :: MonadFail m => String -> m a
```
repl

### 2.4.6 class Monad как расширение Applicative

Монада это обязательно аппликативный функтор,
поэтому у монады базовый класс: Applicative.
Аппликатив это, в совю очередь, функтор.

Не все апликативы могут быть расширены до монады.
Applicative ZipList в монаду не расширяется, например.
```hs

-- Базовый интерфейс монад
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>):: m a -> m b -> m b
    m >> n = m >>= \_ -> n
    fail :: String -> m a

-- Определение монады через аппликатив (fmap, pure, <*>)
-- по сути, реализовать надо только bind
class (Applicative m) => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b

    (>>):: m a -> m b -> m b
    (>>) = (*>) -- кастрированный bind, семантика повторяет "правый ап" аппликатива и это правильная реализация
    m >> n = m >>= \_ -> n -- но вмешались "исторические причины": аппликатив внедрили после монад и
    -- реализацию аппликатива пишут (часто) опираясь на реализацию монады (Димы А. на них нет)

    fail :: String -> m a -- deprecated?

```
repl

> мы знаем только два «интересных» примера аппликативных функторов, не являющихся монадами
один из них это `ZipList` ... (второй `Compose`)

```hs
https://stepik.org/lesson/28881/step/7?unit=9913
TODO
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
instance Monad PrsE where
  (>>=) = undefined

-- solution

```
test [парсер из первого модуля](./chapter1.md#147-parser-lib-satisfy-lower-char-digit-multiplication)

### 2.4.8 вывод `applied over <*>` из монадического `bind >>=`

Из траверсабла дефолтом выводятся его базовые классы фолдабл и функтор.
Похожим образом, из реализации монады выводятся аппликатив и функтор.

Рассмотрим такую реализацию (при наличии монады определить оператор applied over)
```hs
-- имеем реализацию для интерфейса
class (Applicative m) => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    fail :: String -> m a

-- нужно вывести реализацию для аппликатива
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- вот так это делается
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap fs xs = do {
    f <- fs
    x <- xs
    return $ f x
}
-- как и положено аппликативу, выполняются вычисления и эффекты в нужном порядке
-- но никаких дополнительных (монадических) манипуляций эффектами (структурой) нет.
-- Предыдущее вычисление не влияет на структуру следующего

-- упражнение по рефакторингу монадической цепочки
ap fs xs = fs >>= (\f -> xs >>= (\x -> return $ f x))
ap fs xs = fs >>= (\f -> xs >>= (\x -> (return . f) x))
ap fs xs = fs >>= (\f -> xs >>= (return . f))
ap fs xs = fs >>= \f -> xs >>= return . f

```
repl

### 2.4.9 вывод `fmap <$>` из монадического `bind >>=`

Продолжим, реализуем функтор имея реализацию монады.
Такая функция (при ограничении монады) называется liftM
```hs
-- надо получить
fmap :: Functor f => (a -> b) -> f a -> f b
-- существует библиотечная функция
liftM :: (Monad m) => (a -> b) -> m a -> m b

ghci> :t liftM
liftM :: Monad m => (a1 -> r) -> m a1 -> m r

-- как она реализована
liftM f xs = do {
    x <- xs
    return $ f x
}
-- без сахара ду-нотации:
liftM f xs = xs >>= \x -> return $ f x

-- докажем, что законы функтора выполняются, при выполнении законов монад
1. left identity   `return a >>= k  =  k a`
2. right identity  `m >>= return    =  m`
3. associativity   `m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h`

-- законы функтора
1. identity         `fmap id = id`
2. composition      `fmap (g . h) = (fmap g) . (fmap h)` -- free theorem in hask

-- proof
liftM id xs
xs >>= \x -> return (id x)
xs >>= \x -> return x   -- eta
xs >>= return           -- right identity
xs

-- есть лифты для большего количества аргументов
liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f xs ys = do {
    x <- xs
    y <- ys
    return (f x y)
}
```
repl

```hs
https://stepik.org/lesson/28881/step/10?unit=9913
TODO
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
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC = undefined

-- solution
-- первая под-задача из трех частей комплексной задачи

```
test

```hs
https://stepik.org/lesson/28881/step/11?unit=9913
TODO
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
concatOC :: OddC (OddC a) -> OddC a
concatOC = undefined

-- solution
-- вторая под-задача из трех частей комплексной задачи

```
test

```hs
https://stepik.org/lesson/28881/step/12?unit=9913
TODO
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

-- solution
-- третья под-задача из трех частей комплексной задачи
-- имея concat и fmap можно получить Monad

```
test

### 2.4.13 Monad vs MonadFail

Monad fail
это "протечка абстракции", эта функция пользователем не вызывается,
это ручка для подстановки своей реализации для случая облома пар.мат.
внутри bind, когда лямбда ломается.
```hs
-- Где возникает необходимость в fail

do patmat <- computation
    more
-- превращается в
let f patmat = more
    f _ = fail "..."
in computation >>= f
-- там где стрелка клейсли встраивается в пайплайн

f xs = do {
    True <- xs -- вот тут пат.мат. ломается с вызовом fail
    return ()
}

f $ Just False -- Nothing -- сработал фейл

-- проблема в том, что мы не можем гарантировать тотальность таких вычислений,
-- зависит от конкретной реализации fail

-- уберем фейл из монады и сделаем
class (Monad m) => MonadFail m where
    fail :: String -> m a

-- Потом компилятор, при обнаружении "плохих" опровержимых образцов, требует контекста MonadFail.
-- Иначе можно пользоваться контекстом Monad и этот кусок кода признается тотальным.
```
repl

> A Monad without a MonadFail instance may only be used in conjunction with pattern that always match,
such as newtypes, tuples, data types with only a single data constructor,
and irrefutable patterns (`~pat`).
https://hoogle.haskell.org/?hoogle=MonadFail&scope=set%3Ahaskell-platform

## chapter 2.5, Классы типов Alternative и MonadPlus

https://stepik.org/lesson/30721/step/1?unit=11244

- Класс типов MonadPlus
- Представитель MonadPlus для списка
- Законы MonadPlus
- Использование MonadPlus: guard
- Использование MonadPlus: msum
- Использование MonadPlus: mfilter

### 2.5.2 class MonadPlus как расширение Alternative

На языке парсера, аппликатив означал "применить и этот парсер и тот", тип "произведения".
Альтернатив (расширение аппликатива) же значил "применить этот или тот". Тип "суммы".
И были законы, наподобие дистрибутивности умножения над сложением.
Законы для тайпкласса альтернатив это законы моноида (правая, левая единица, ассоциативность).

А что насчет монад, как здесь обстоят дела с альтернативом?
Для монад есть класс `MonadPlus`.
Задача: добавить к тайпклассу монад дополнительную структуру - моноид.
```hs
-- ранее мы рассмотрели альтернатив
class (Applicative f) => Alternative f where -- интерфейс моноидальной структуры
    empty :: f a -- нейтральный элемент
    (<|>) :: f a -> f a -> f a -- ассоциативная бинарная операция

-- теперь посмотрим на производное: монадплюс
class (Alternative m, Monad m) => MonadPlus m where
    mzero :: m a
    mzero = empty
    mplus :: m a -> m a -> m a
    mplus = (<|>)

-- Рассмотрим инстансы тайпкласса монадплюс

-- альтернатива для мейби это First реализация моноида: из цепочки выходит первый не-пустой
instance Alternative Maybe where -- наложить ограничение на `a` мы не можем, класс не позволяет добавить параметр `a`
    empty = Nothing
    Nothing <|> r = r
    l <|> _ = l
-- без ограничений на `a` это единственная разумная реализация (First)
-- первый не-пустой, органично ложится на тип-суммы (из двух конструкторов)
ghci> Nothing <|> Just 3 <|> Just 5 <|> Nothing
Just 3

instance MonadPlus Maybe -- дефолтная реализация закрывает наши нужды

ghci> Nothing `mplus` Just 3 `mplus` Just 5 `mplus` Nothing
Just 3
```
repl

### 2.5.3 `instance MonadPlus []`

Не для всех типов монад есть возможность (или смысл) добавить моноидальную структуру.
Для списка можно, будет хорошая иллюстрация: как аппликатив работает умножением а альтернатив работает сложением.
```hs
-- ранее рассматривали
instance Alternative [] where -- реализация альтернативы для списка
    empty = []
    (<|>) = (++) -- список позволяет комбинировать: склеить два списка
-- альтернатива для списка повторяет поведение моноида для списка

-- теперь добавляется
instance MonadPlus []

-- examples

ghci> [1,2,3] <|> [4,5] -- alternative
[1,2,3,4,5] -- сумма (количества, 3+2), речь идет о "структуре контейнера", об эффектах
-- эффект списка это количество элеентов

ghci> [1,2,3] *> [4,5] -- applicative
[4,5,4,5,4,5] -- произведение (количество, 3*2)

ghci> [1,2,3] `mplus` [4,5] -- MonadPlus
[1,2,3,4,5] -- сумма (количество 3+2)

ghci> [1,2,3] >> [4,5] -- bind
[4,5,4,5,4,5] -- произведение (количество, 3*2)

-- практически всегда монадплюс имеет дефолтную реализацию через альтернатив
```
repl

### 2.5.4 MonadPlus laws

Так в чем заключается различие альтернатив и монадплюс?

Альтернатив это моноид, добавленный к аппликативной структуре.
Законы моноида + законы аппликатива (сложение, умножение, дистрибутивность)

Монадплюс это монада с добавленным моноидным поведением.
Соответственно, набор законов это отражает
```hs
-- 1. Left Zero
mzero >>= k = mzero -- похоже на аппликативный закон: empty <*> a = empty

-- 2. Right Zero
v >> mzero = v

-- вот эти законы выполняются не для всех инстансов монадплюс:
-- другими словами: должен выполняться хотя бы один из этих двух законов

-- 3. Left Distribution
(a `mplus` b) >>= k = (a >>= k) `mplus` (b >>= k)

-- 4. Left Catch Law
(return a) `mplus` b = return a

-- examples

-- для списка №4 не выполняется
ghci> return 2 `mplus` [1,3]
[2,1,3]

-- для мейби №4 выполняется
ghci> return 2 `mplus` Just 3
Just 2
```
repl

Законы можно разделить на три вида:
- законы формирования алгебраической структуры (must);
- законы - свободные теоремы (следование ограничениям Хаскел);
- законы - свойства, выводимые из ...

Haskell не может проверить законы, следить за этим должен программист.

Kmett:
> MonadPlus is a stronger claim than Alternative, 
which in turn is a stronger claim than Monoid, 
and while the MonadPlus and Alternative instances for a type should be related, 
the Monoid may be (and sometimes is) something completely different.
https://stackoverflow.com/a/10168111

```hs
https://stepik.org/lesson/30721/step/5?unit=11244
TODO
{--
Выполняются ли для стандартных представителей 
`Applicative`, `Alternative`, `Monad` и `MonadPlus` 
типа данных `Maybe` 
следующие законы дистрибутивности: 

(u <|> v) <*> w       =    u <*> w <|> v <*> w

(u `mplus` v) >>= k   =    (u >>= k) `mplus` (v >>= k)

Если нет, то приведите контрпример, если да, то доказательство.
Предполагается, что расходимости отсутствуют.
--}
- Write an answer
- Send your best submission to review
- Review submissions
- Wait for reviews of your solution
- Get points, max score is 3 points 

-- solution

```
test

```hs
https://stepik.org/lesson/30721/step/6?unit=11244
TODO
{--
Предположим мы сделали парсер

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

представителем классов типов `Alternative` следующим образом

instance Alternative PrsE where
  empty = PrsE f where 
    f _ = Left "empty alternative"
  p <|> q = PrsE f where 
    f s = let ps = runPrsE p s 
      in if null ps 
         then runPrsE q s 
         else ps

Эта реализация нарушает закон дистрибутивности для `Alternative`:

GHCi> runPrsE ((charE 'A' <|> charE 'B') *> charE 'C') "ABC"
Left "unexpected B"
GHCi> runPrsE (charE 'A' *> charE 'C' <|> charE 'B' *> charE 'C') "ABC"
Left "unexpected A"

От какого парсера приходит сообщение об ошибке в первом и втором примерах?

Select all correct options from the list
(1) charE 'A'
(1) charE 'B'
(1) charE 'C'
(2) charE 'A'
(2) charE 'B'
(2) левый charE 'C'
(2) правый charE 'C'
--}

-- solution

```
test

```hs
https://stepik.org/lesson/30721/step/7?unit=11244
TODO
{--
Реализуем улучшенную версию парсера PrsE

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }
parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

Этот парсер получил дополнительный целочисленный параметр в аргументе и в возвращаемом значении. 
С помощью этого параметра мы сможем отслеживать и передвигать текущую позицию в разбираемой строке и 
сообщать о ней пользователю в случае ошибки:

GHCi> charEP c = satisfyEP (== c)
GHCi> runPrsEP (charEP 'A') 0 "ABC"
(1,Right ('A',"BC"))
> runPrsEP (charEP 'A') 41 "BCD"
(42,Left "pos 42: unexpected B")
> runPrsEP (charEP 'A') 41 ""
(42,Left "pos 42: unexpected end of input")

Вспомогательная функция `parseEP` дает возможность вызывать парсер более удобным образом 
по сравнению с `runPrsEP`, скрывая технические детали:

GHCi> parseEP (charEP 'A') "ABC"
Right ('A',"BC")
GHCi> parseEP (charEP 'A') "BCD"
Left "pos 1: unexpected B"
GHCi> parseEP (charEP 'A') ""
Left "pos 1: unexpected end of input"

Реализуйте функцию 
satisfyEP :: (Char -> Bool) -> PrsEP Char
обеспечивающую описанное выше поведение.
--}
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/30721/step/8?unit=11244
TODO
{--
Сделайте парсер

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }
parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

представителем классов типов `Functor` и `Applicative`, обеспечив следующее поведение:

GHCi> runPrsEP (pure 42) 0 "ABCDEFG"
(0,Right (42,"ABCDEFG"))
GHCi> charEP c = satisfyEP (== c)
GHCi> anyEP = satisfyEP (const True)
GHCi> testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
GHCi> runPrsEP testP 0 "ABCDE"
(3,Right (('A','C'),"DE"))
GHCi> parseEP testP "BCDE"
Left "pos 2: unexpected C"
GHCi> parseEP testP ""
Left "pos 1: unexpected end of input"
GHCi> parseEP testP "B"
Left "pos 2: unexpected end of input"
--}

-- solution

```
test

```hs
https://stepik.org/lesson/30721/step/9?unit=11244
TODO
{--
Сделайте парсер

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }
parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

представителем класса типов `Alternative`, 
обеспечив следующее поведение для пары неудачных альтернатив: 
сообщение об ошибке возвращается из той альтернативы, которой удалось распарсить входную строку глубже.

GHCi> runPrsEP empty 0 "ABCDEFG"
(0,Left "pos 0: empty alternative")
GHCi> charEP c = satisfyEP (== c)
GHCi> tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
Left "pos 2: unexpected E"
--}

-- solution

```
test

### 2.5.10 guard (Alternative)

В чем польза монадплюс (Alternative)?
В облегчении кодирования некоторых операций
```hs
guard :: MonadPlus m => Bool -> m () -- было
guard :: Alternative f => Bool -> f () -- стало (более слабое ограничение)
guard True = pure () -- return () -- юнит-тайп, контейнер (контекст) непустой
guard False = empty -- mzero, контейнер (контекст) пустой, закон Left Zero здесь зануляет следующие стрелки Клейсли
-- гард в монадическом вычислении либо зануляет следующие шаги пайплайна, либо пропускает без изменений

-- 1. Left Zero
mzero >>= k = mzero -- похоже на аппликативный закон: empty <*> a = empty

pythags = do
    z <- [1 ..]
    x <- [1 .. z]
    y <- [x .. z]
    guard (x^2 + y^2 == z^2) -- управление эффектами на основе булева значения
    -- для списка будет эффект одного элемента или пустого списка
    return (x, y, z)

ghci> take 5 pythags 
[(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]

-- если засахарить, то вот так:
pythags = [(x,y,z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]
```
repl

### 2.5.11 asum (Alternative)

Следующая полезная вещь: свертка на моноидной структуре,
есть целый набор подобных функций
```hs

-- для моноидов

mconcat :: Monoid m => [m] -> m
mconcat = foldr mappend mempty

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

-- для монадплюс

msum :: (Foldable t, MonadPlus m) => t m -> m
msum = asum -- стало (более общая, слабее ограничение)
msum = foldr mplus mzero -- было

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty -- пользуйтесь asum, это более современное и правильное средство

-- examples

-- фолдабл это список, альтернатив это мейби
ghci> msum [Nothing, Just 3, Just 5, Nothing]
Just 3
```
repl

### 2.5.12 mfilter (MonadPlus)

Еще одна полезная вешь (в монадплюс или альтернатив):
на основе предиката влияет на пайплайн (похоже на guard)
```hs
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p ma = do
    a <- ma
    if p a
    then return a
    else mzero

ghci> mfilter (> 3) (Just 4)
Just 4
ghci> mfilter (> 3) (Just 3)
Nothing

mfilter меняет структуру контейнера на основе значений из контенера,
такое поведение доступно только в монаде,
поэтому в альтернатив поднять эту функцию нельзя

```
repl



grep `TODO` markers, fix it.
