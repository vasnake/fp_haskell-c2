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
-- что если: дерево бесконечно, пусто, не пусто но без значений, ...?

```
test

### 2.1.11 Foldable (foldr', foldr1, and, or, any, all, concat, concatMap)

Продолжим смотрить на предоставляемые фолдаблом функции
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

Хотим реализацию foldr через foldMap, ибо `MCD: foldMap | foldr`
а реализацию foldMap через foldr мы уже видели.

foldl реализуется через foldMap, кстати.

Предварительно необходимо понять "эндоморфизм".
Эндоморфизм это стрелка из сета в этот же сет.
Область определений и область значений функции совпадают.
Это стрелка из А в А. (инт в инт, строка в строку, ...).

На типе Endo можно сделать моноид на операции "композиция": композиция функций, mappend aka `<>`.
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

```hs
https://stepik.org/lesson/30427/step/13?unit=11044
TODO
{--
Реализуйте функцию

mkEndo :: Foldable t => t (a -> a) -> Endo a

принимающую контейнер функций и последовательно сцепляющую элементы этого контейнера с помощью композиции,
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

```
test

### 2.1.14 реализация foldr через foldMap

фолдмэп реализован через фолдр, фолдр реализован через фолдмэп.
Достаточно предоставить свою реализацию любой из этих ф. чтобы сделать свой фолдабл.

Используем эндоморфизм для реализации foldr через foldMap
```hs
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m -- преобразование каждого а в эм с последующим суммированием
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini ta = appEndo (foldMap (Endo . f) ta) ini

ghci> :t Endo
Endo :: (a -> a) -> Endo a

ghci> :t appEndo 
appEndo :: Endo a -> a -> a

-- разбор устройства foldr-using-foldMap

ini :: b
f :: a -> (b -> b) -- для удобства рассуждений, f берет а и возвращает эндоморфизм
(Endo . f) :: a -> Endo b -- а эта композиция выглядит как аргумент для foldMap: (a -> m)
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

### 2.1.15 реализация foldl через foldMap

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
    foldl f ini ta = appEndo (getDual (foldMap (Dual . Endo . (flip f)) ta)) ini

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

```hs
https://stepik.org/lesson/30427/step/16?unit=11044
TODO
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

Как можно сочетать Applicative (Alternative) и Foldable?
Рассмотрим такие сочетания, проанализируем и, в итоге, выведем Traversable
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

-- example

ghci> fmap Just testTree -- получим `t (f a)` где фолдабл это дерево, альтернатив это мэйби, а это число
Branch (Branch (Branch Nil (Just 1) Nil) (Just 2) (Branch Nil (Just 3) Nil)) (Just 4) (Branch Nil (Just 5) Nil)

ghci> asum $ fmap Just testTree
Just 4 -- семантика First, при обходе пре-ордер
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
-- "список" аппликативов связывается в цепочку (пайплайн) аппликативов на операторе "насрать на значение, сделай эффекты"

ghci> :t (*>) -- вместо mappend, правая `applied over', для попадания в сигнатуру foldr
(*>) :: Applicative f => f a -> f b -> f b
-- выполняет эффекты как и полноценный ап, но игнорирует левое значение

ghci> :t (<*>)
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

```hs
{--
Предположим для двоичного дерева

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

реализован представитель класса типов `Foldable`
обеспечивающий стратегию обхода pre-order traversal.

Какую строку вернет следующий вызов

GHCi> tree = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
GHCi> fst $ sequenceA_ $ (\x -> (show x,x)) <$> tree
--}

-- solution

"21435"

-- почему?

ghci> toList tree
[2,1,4,3,5] -- pre-order

ghci> sequenceA_ $ (\x -> (show x,x)) <$> tree -- в значения дерева записали пары (строка, значение)
("21435",()) -- после прохода сиквенсом-для-эффектов, получим лог: конкатенацию строк в порядке обхода
-- ибо первые значения пар это эффект лога, см. семантику аппликатива для пары
```
test

### 2.2.5 traverse_ (foldMap for Applicative)

foldMap для Applicative называется `traverse_`
```hs
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
-- мы не хотим превращать любой фолдабл в список, это ну универсально

-- хотим внутри аппликатива результата получить тот-же фолдабл, что и на входе,
-- сохранить структуру фолдабл
sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)

-- увы, фолдабл не позволяет восстановить (клонировать) структуру,
-- в его интерфейсе нет такого метода.
-- Поэтому нам нужен Traversable
```
repl

```hs
https://stepik.org/lesson/30428/step/7?unit=11045
TODO
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
[Just 5,Just 7,Just 25] -- аппликатив это список
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
    fmap g (x, y) = (,) x (g y)

instance Traversable ((,) s) where
    traverse :: Applicative f => (a -> f b) -> (s, a) -> f (s, b)
    traverse g (x, y) = (pure ((,) x)) <*> (g y) -- аналогично: поднять конструктор пары в аппликатив, ап его на результат (g y)
    traverse g (x, y) = ((,) x) <$> (g y) -- согласно закону левого pure для аппликатива

-- examples

ghci> sequenceA ("foo", Just 42)
Just ("foo",42)

ghci> sequenceA $ sequenceA ("foo", Just 42)
("foo",Just 42)
```
repl

```hs
https://stepik.org/lesson/30428/step/11?unit=11045
TODO
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

```
test

```hs
https://stepik.org/lesson/30428/step/12?unit=11045
TODO
{--
Сделайте тип данных `Result`

data Result a = Ok a | Error String deriving (Eq,Show)

представителем класса типов `Traversable` (и всех других необходимых классов типов).

GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
[Ok 7,Ok 3]
GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
[Error "!!!"]
--}

-- solution

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

```hs
https://stepik.org/lesson/30428/step/14?unit=11045
TODO
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

```
test

```hs
https://stepik.org/lesson/30428/step/15?unit=11045
TODO
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

### 2.3.2 Traverse identity law

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

### 2.3.3 Traverse composition law

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

### 2.3.4
```hs

```
repl



grep `TODO` markers, fix it.
