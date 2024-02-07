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

Траверсабл не только производит эффекты (аппликативные), но и клонирует структуру, по которой итерируется
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


grep `TODO` markers, fix it.
