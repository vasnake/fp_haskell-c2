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

```
test



grep `TODO` markers, fix it.
