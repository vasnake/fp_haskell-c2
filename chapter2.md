# FP Haskell, chapter 2, управление эффектами

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-2/test-effects.hs)

Мотивация: ???

definitions: ???

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
```
repl

### 2.1.3 Foldable `[]`, Maybe, `(,)`

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

```hs
https://stepik.org/lesson/30427/step/4?unit=11044
TODO
{--
Сделайте тип

data Triple a = Tr a a a  deriving (Eq,Show)

представителем класса типов `Foldable`:

GHCi> foldr (++) "!!" (Tr "ab" "cd" "efg")
"abcdefg!!"
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
--}

-- solution
foldr ...

```
test

### 2.1.5 Foldable Tree

Свертка дерева уже интереснее. Разные варианты обхода (in-order, pre-order, post-order)
```hs
import Data.Foldable
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- in-order: 1 2 3 4 5

instance Foldable Tree where
    foldr f ini Nil = ini    
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

```hs
https://stepik.org/lesson/30427/step/6?unit=11044
TODO
{--
Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева
https://en.wikipedia.org/wiki/Tree_traversal

Сделайте двоичное дерево

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

```
test

### 2.1.7 Foldable (fold, foldMap)

У Foldable есть (кроме фолдл, фолдр) другие функции.
Некоторые требуют чтобы значения были в моноиде.
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

-- функции требующие от значения быть в моноиде:

    fold :: Monoid m => t m -> m -- свертка "списка" моноидов
    fold = foldr mappend mempty -- mconcat

    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием
    -- foldMap f ta = let tm = fmap f ta in fold tm -- увы, так нельзя, тип t не заявлен как Functor, у него нет fmap
    foldMap f ta = foldr (mappend . f) mempty ta
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

```hs
https://stepik.org/lesson/30427/step/8?unit=11044
TODO
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
--}

-- solution

```
test

### 2.1.9 Foldable (sum, product, null, toList, length, maximum, minimum, elem)

Посмотрим на интерфейс, получаемый от Foldable
```hs
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    fold :: Monoid m => t m -> m -- свертка "списка" моноидов
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием

-- другие полезные функции

    sum :: Num a => t a -> a
    sum = getSum . foldMap Sum

    product :: Num a => t a -> a
    product = getProduct . foldMap Product

    null :: t a -> Bool
    null = foldr (\ _ _ -> False) True -- лямбда сработает на первом же элементе и даст не-нулл

    toList :: t a -> [a]
    toList = foldr (:) []

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

```hs
https://stepik.org/lesson/30427/step/10?unit=11044
TODO
```
test



grep `TODO` markers, fix it.
