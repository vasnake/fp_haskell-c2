# FP Haskell, chapter 1, аппликативные функторы

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-1/test-applicative.hs)

Мотивация: ???

definitions: ???

## chapter 1.1, Определение аппликативного функтора

https://stepik.org/lesson/28880/step/1?unit=9912

- Стандартные однопараметрические типы
- Представители класса типов Functor для стандартных однопараметрических типов
- Законы класса типов Functor
- Законы класса типов Functor для рекурсивных типов
- Класс типов Pointed
- Представители класса типов Pointed
- Класс типов Apply
- Класс типов Applicative
- Законы класса Applicative

### 1.1.2 вспоминаем параметризованные типы данных

Параметризованные типы данных, типы высших порядков, конструкторы типов.
Про них говорят "контейнеры", "контексты вычислений".
Для них есть много разных интерфейсов (e.g. functor, monad), позволяющих в модульность и композицию.

Аппликативный функтор лежит на шкале между функтором и монадой.
При этом функтор не меняет структуру контейнера, а монада меняет.
```hs
-- Напомним, какие (одно)параметрические типы данных мы видели

class Functor f where -- переменная `f` используется как функция над типами, у нее должен быть "стрелочный кайнд": `* -> *`
    fmap :: (a -> b) -> f a -> f b -- fmap or <$>

class Monad m where
    return :: a -> m a -- pure
    (>>=) :: m a -> (a -> m b) -> m b -- bind
    (>>) :: m a -> m b -> m b -- then, sequence: -- mx >> my = mx >>= (\ _ -> my)
    fail :: String -> m a

-- никакой дополнительной функциональности, newtype в рантайме разворачивается в свое представление (типа макроса)
newtype Identity a = Identity { runIdentity :: a }

-- тип-сумма, семантика опционального значения а
data Maybe a = Nothing | Just a

-- псевдо-запись, формально инвалидная: семантика пары значений, тип-произведение
data (,) a b = (a, b)

-- тип-сумма, "расширенный мэйби", семантика результата в двух вариантах, ошибка слева или значение справа
data Either a b = Left a | Right b

-- самый хитрожопый, рекурсивный тип данных, список; одновременно и тип-сумма и тип-произведение; семантика может быть навешана любая
data [] a = [] | a : [a]

-- тоже рекурсивный тип, сумма и произведение одновременно
data Tree a = Leaf | Branch (Tree a) a (Tree a)
```
repl

### 1.1.3 Functor, fmap

Реализуем функтор для наших типов.
Функтор: структура контейнера не должна меняться (контекст не меняется при использовании fmap).
```hs
-- функтор поднимает функцию в контекст
class Functor f where -- переменная `f` используется как функция над типами, у нее должен быть "стрелочный кайнд": `* -> *`
    fmap :: (a -> b) -> f a -> f b -- fmap or <$>

-- связываем аргументы с переменными и "протаскиваем" функцию через "контейнер"
instance Functor Identity where
    fmap g (Identity x) = Identity $ g x -- два параметра, вернуть надо Identity где функция применена к значению х

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just $ g x

instance Functor [] where
    fmap = map -- для списка уже есть реализация fmap

-- чтобы сделать функтор из двух-параметрических типов, надо первый тип связать в переменную,
-- тогда обеспечивается соответствие по kind
instance Functor (Either e) where
    fmap _ (Left x) = Left x
    fmap g (Right x) = Right $ g x

instance Functor ((,) s) where
    fmap g (x, y) = (x, g y)

-- функтор над стрелочным типом, семантика: выполнить ф. с чтением данных из environment
-- стрелочный тип увеличивает арность результирующего выражения (при использовании нужет доп.параметр)
instance Functor ((->) e) where
    fmap = (.) -- композиция, сначала выполнится ф. из первого аргумента, затем на ее результате выполнится ф. из второго аргумента
```
repl

```hs
{--
В модуле `Data.Functor` определен оператор `<$>`, являющийся инфиксным аналогом функции `fmap`

GHCi> :info <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
        -- Defined in `Data.Functor'
infixl 4 <$>

В выражении `succ <$> "abc"` этот оператор имеет тип `(Char -> Char) -> [Char] -> [Char]`
Какой тип имеет первое (левое) вхождение этого оператора в выражении `succ <$> succ <$> "abc"`
--}

-- solution

Выражение
succ <$> succ <$> "abc"
Содержит два оператора `fmap` aka `<$>`.
Оператор левоассоциативен, первое вхождение (согласно лекции) дает композицию функций `succ . succ`
а это (в контексте переданного аргумента) дает тип `Char -> Char`.
Но вопрос составлен так, что моя интерпретация ошибочна и такой ответ не проходит.

Правильный ответ: `(Char -> Char) -> (Char -> Char) -> Char -> Char`
Попробую сформулировать вопрос под такой ответ:
Если бы нам надо было вместо foo (в выражении`foo <$> "abc"`) подставить выражение `succ <$> succ`
в виде функции двух аргументов, то какая была бы сигнатура этой функции?

В таком случае, ответ
`(Char -> Char) -> (Char -> Char) -> Char -> Char`
понятен, ибо композиция двух функций именно так и выглядит в данном случае,
берем две функции `Char -> Char` и получаем третью ф. `Char -> Char`, скобки справа можно убрать (каррирование).
```
test

```hs
https://stepik.org/lesson/28880/step/5?unit=9912
TODO
{--
Сделайте типы данных `Arr2 e1 e2` и `Arr3 e1 e2 e3` представителями класса типов `Functor`

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

Эти типы инкапсулируют вычисление с двумя и тремя независимыми окружениями соответственно

GHCi> getArr2 (fmap length (Arr2 take)) 10 "abc"
3
GHCi> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
[33,44]
--}
newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap = undefined

instance Functor (Arr3 e1 e2 e3) where
  fmap = undefined

-- solution

```
test

### 1.1.6 законы функтора

Тайп-класс Functor требует выполнения законов, если законы не выполняются, то это не функтор,
хотя по сигнатуре (типам) может все совпадать.
Структура контейнера должна сохраняться.
Два закона.
```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- given: x :: Functor
-- first law: fmap id, факт поднятия ф. в контекст не влияет на контекст
fmap id x = x
-- second law: композиция, количество поднятий не влияет на контекст
fmap f (fmap g x) = fmap (f . g) x

-- доказательства для разных реализаций

data Either a b = Left a | Right b
instance Functor (Either e) where
    fmap _ (Left x) = Left x
    fmap g (Right x) = Right $ g x

-- запишем и выполним подстановки для всех веток fmap

-- law #1
fmap id x = x
fmap id (Left a) = Left a
Left a = Left a

fmap id (Right b) = Right b
Right $ id b
Right b = Right b

-- law 2
fmap f (fmap g x) = fmap (f . g) x
fmap f (fmap g (Left a)) = fmap (f . g) (Left a)
fmap f (Left a) = fmap (f . g) (Left a)
Left a = fmap (f . g) (Left a)
Left a = Left a

fmap f (fmap g x) = fmap (f . g) x
fmap f (fmap g (Right b)) = fmap (f . g) (Right b)
fmap f (Right (g b)) = fmap (f . g) (Right b)
Right (f (g b)) = fmap (f . g) (Right b)
Right (f (g b)) = Right ((f . g) b)
Right (f (g b)) = Right (f (g b))
```
repl

```hs
{--
Самостоятельно докажите выполнение первого `(fmap id = id)`
и второго `(fmap f . fmap g = fmap (f . g))`
законов функторов для 
функтора частично примененной функциональной стрелки `(->) e`

Отметьте те свойства оператора композиции функций, которыми вы воспользовались

Select all correct options from the list
- id является правым нейтральным элементом для композиции.
- id является левым нейтральным элементом для композиции.
- Композиция не коммутативна.
- Композиция ассоциативна.
--}

-- solution

- id является правым нейтральным элементом для композиции.
- (да) id является левым нейтральным элементом для композиции.
- Композиция не коммутативна.
- (да) Композиция ассоциативна.

1) fmap id === id

id <$> h -- fmap id h = h
~> id . h
~> h -- id левый нейтральный

2) fmap f . fmap g === fmap (f . g)

f <$> (g <$> h) -- левая часть
~> f <$> (g . h)
~> f . (g . h)
~> f . g . h -- ассоциативность композиции

(f . g) <$> h -- правая часть
~> (f . g) . h
~> f . g . h -- ассоциативность композиции
```
test

### 1.1.8 доказательство законов для рекурсивных типов данных

Продолжаем про законы функтора.
Доказательство для не-рекурсивных типов выглядит тривиально, подставляем и сокращаем до победного конца.

С рекурсивными типами сложнее.
Доказать законы подстановкой для рекурсивных типов данных можно через прием: структурная индукция.
Базовые случаи, индукционная гипотеза (для списка: если на `xs` закон выполняется, выполняется ли он на (`x:xs`)?).

Формально структурная индукция сводится к индукции по натуральным числам, оставим это математикам.
```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-- first law: fmap id, факт поднятия ф. в контекст не влияет на контекст
fmap id x = x
-- second law: композиция, количество поднятий не влияет на контекст
fmap f (fmap g x) = fmap (f . g) x

data [] a = [] | a : [a]
instance Functor [] where
    fmap = map -- для списка уже есть реализация fmap

map _ [] = []
map g (x:xs) = (g x) : (map g xs)

-- докажем для списка первый закон

-- базовый случай
fmap id [] = []
[] = []

-- индукционная гипотеза, IH: предположим, что для xs выполняется закон
fmap id (x:xs) = (x:xs)
(id x) : (fmap id xs)
x : (fmap id xs) -- IH
x : xs = x: xs
```
repl

```hs
{--
Докажите выполнение второго закона функторов для функтора списка
`fmap f (fmap g xs) = fmap (f . g) xs`
Предполагается, что все списки конечны и не содержат расходимостей

- Write an answer
- Send your best submission to review
- Review submissions
- Wait for reviews of your solution
- Get points, max score is 2 points
--}

-- solution

> в Haskell для любого типа, для которого определен и выполняется первый закон функтора, выполняется и второй
https://www.schoolofhaskell.com/user/edwardk/snippets/fmap

-- докажем для списка первый закон `fmap id x = x`

-- базовый случай
fmap id [] = []
[] = []

-- индукционная гипотеза, IH: предположим, что для xs выполняется 1 закон
fmap id (x:xs) = (x:xs)
(id x) : (fmap id xs) = (x:xs)
x : (fmap id xs) = (x:xs) -- IH: fmap id xs = xs
x:xs = x:xs

--  Comment from teacher

-- второй закон: `fmap f (fmap g xs) = fmap (f . g) xs`
Доказываем индукцией по списку xs⁡
База индукции: xs⁡ = []

fmap f (fmap g []) ≡ fmap f []  -- def fmap
                   ≡ []         -- def fmap

fmap (f . g) [] ≡ []  -- def fmap

Обе части равенства равны одному и тому же.

Индукционный переход: xs⁡ = y⁡:ys
По предположению индукции, для ys⁡ утверждение верно

fmap f (fmap g (y : ys)) ≡ fmap f (g y : fmap g ys))     -- def fmap
                         ≡ f (g y) : fmap f (fmap g ys)  -- def fmap

fmap (f . g) (y : ys) ≡ (f . g) y : fmap (f . g) ys  -- def fmap
                      ≡ f (g y) : fmap (f . g) ys    -- def (.)
                      ≡ f (g y) : fmap f (fmap g ys)

```
test

### 1.1.10 Applicative (Pointed pure)

Applicative это наследник Functor, расширяющий функциональность функтора.
Добавляет он две функции, которые будут изучены в рамках классов `Pointed` и `Apply`:
function `pure`;
```hs
import Prelude hiding (pure)

-- функция pure aka return: поднятие значения в функтор (в контекст)
-- тривиальный конструктор типа
class (Functor f) => Pointed f where -- type-class - наследник функтора
    pure :: a -> f a -- aka: singleton, return, unit, point

-- реализации для разных типов
-- вариативность реализаций ограничена законами (требованиями) аппликатива

instance Pointed Maybe where
    pure x = Just x -- не такой тривиальный как Nothing, но семантически верный

instance Pointed [] where
    pure x = [x] -- тривиальный конструктор типа

instance Pointed (Either e) where
    pure x = Right x -- лефт не подходит, только так можно тривиально построить значение типа `f a`

-- pure полиморфна по контейнеру, она определена для разных контекстов
```
repl

Q
> В чём смысл названия "Pointed"?
A
> если мы имеем отмеченный (pointed) элемент в типе `a`,
то для представителя класса типов `Pointed`
гарантируется наличие соответствующего отмеченного элемента в типе `f a`
Классов типов `Pointed` и `Apply` нет в стандартной библиотеке. Мы их ввели для учебных целей

### 1.1.11 pure, free theorem

Продолжение про Pointed
```hs
-- pure полиморфна по контейнеру, она определена для разных контекстов
-- подскажем компилятору тип контекста
pure True :: [Bool]
pure True :: Maybe Bool

-- пойнтед для стрелки это стрелка из енв. в значение
instance Pointed ((->) e) where
    pure = const -- a -> fa = a -> (e -> a) = a -> e -> a
    pure x e = x -- alternative
    pure x = \ e -> x

instance Pointed ((,) s) where
    pure x = (undefined, x) -- херня, не надо так

instance (Monoid s) => Pointed ((,) s) where
    pure x = (mempty, x) -- воспользуемся нейтралью моноида эс

ghci> pure True :: (String, Bool)
("",True)

-- закон для typeclass Pointed: поднятие значения в контекст не влияет на значение
fmap g (pure x) = pure (g x)
-- это "свободная теорема", выполняется автоматически для корректной реализации функтора

From the type of a polymorphic function we can derive a theorem that it satisfies.
Every function of the same type satisfies the same theorem.
This provides a free source of useful theorems,
courtesy of Reynolds' abstraction theorem for the polymorphic lambda calculus
Theorems for free / Philip Wadler https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf

```
repl

### 1.1.12 applied over `<*>`, Apply

Перед переходом к аппликативу, рассмотрим проблему (мотивация)
```hs
-- предположим, у нас есть функтор и мы хотим имея два контекста-функтора и функцию двух аргументов,
-- поднять эту функцию в контекст. Не получается? Да, нужна монада и bind для такого фокуса,
-- в do-нотации такое можно записать
fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap3arg :: Functor f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap2arg g as bs = undefined
-- есть только fmap, попробуем применить
fmap2arg :: Functor f => (a -> (b -> c)) -> f a -> f b -> f c
fmap2arg g as bs = fmap g as -- :: f (b -> c)
-- есть стрелка в контексте функтора `f (b -> c)`
-- и есть функтор `f b`
-- как их поженить? Стрелка в контексте и ее аргумент в контексте, ... монада-бинд ...
-- нужна функция: `foo :: Functor f => f (b -> c) -> f b -> f c`
fmap2arg :: Functor f => (a -> (b -> c)) -> f a -> f b -> f c
fmap2arg g as bs = _ (fmap g as) bs -- дырка вместо имени функции

-- дырка позволяет подсмотреть желаемый тип:
-- error: • Found hole: _ :: f (b -> c) -> f b -> f c ...

-- Короче: выяснили, что функтора fmap недостаточно для функций больших арностей в контексте функтора.
-- Нужна функция типа
_ :: f (b -> c) -> f b -> f c
-- Эта функция принадлежит нашему тайпклассу Apply и это оператор `<*>`
import Prelude hiding ((<*>))
infixl 4 <*> -- apply over, ap

class Functor f => Apply f where -- наследует и расширяет функтор
    (<*>) :: f (a -> b) -> f a -> f b

-- реализация для списка, семантика (список функций и список аргументов на входе, что на выходе)?
-- вариант: zip
-- другой: вложенные циклы (для каждой эф, получить список выходов из списка входов, ... списки конкатенировать)

instance Apply [] where -- зипует по самому короткому списку
    (g:gs) <*> (x:xs) = (g x) : (gs <*> xs)
    _ <*> _ = [] -- для пустых

-- теперь реализация лифтов тривиальна, цепочка каррированных вычислений в контексте
fmap2arg :: Apply f => (a -> b -> c) -> f a -> f b -> f c -- liftA2
fmap2arg g as bs = (fmap g as) <*> bs
fmap2arg g as bs = g <$> as <*> bs -- infixl
-- g a b -- если без контекста

fmap3arg :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d -- liftA3
fmap3arg g as bs cs = ((fmap g as) <*> bs) <*> cs
fmap3arg g as bs cs = g <$> as <*> bs <*> cs -- infixl
-- g a b c -- если без контекста
```
repl

### 1.1.13 Control.Applicative, pure law

Сочетание `pure` и `ap` (поднятие значения в контекст и поднятие вычисления в контекст) открывает большое количество разнообразных комбинаций.
Поэтому эти функции собрали в отдельном тайпклассе, `Applicative`.

Закон для `pure` в аппликативе.
```hs
class Functor f => Applicative f where
    pure :: a -> f a -- return
    (<*>) :: f (a -> b) -> f a -> f b -- ap, applied over

-- посмотрим на реализации

-- семантика: вычисления с эффектом, эффект: возможность отсутствия значений
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just g) <*> x = fmap g x -- x :: Functor; g :: a -> b; -- если справа Nothing то fmap сделает Nothing, не надо отдельно это ловить

ghci> Just (+2) <*> Just 5
Just 7

ghci> Just (+) <*> Just 2 <*> Just 5
Just 7

-- pure не должна выполнять эффектов, поэтому pure = Nothing семантически неверно,
-- появление Nothing это и есть эффект Maybe
-- закон для pure (в рамках аппликатива)
fmap g x = pure g <*> x
-- поскольку fmap не меняет структуру контейнера, постольку pure и апаплайд-овер также не меняют структуру контейнера
-- иллюстрация
ghci> pure (+) <*> Just 2 <*> Just 5
Just 7
ghci> (fmap (+) (Just 2)) <*> Just 5
Just 7
ghci> (+) <$> Just 2 <*> Just 5 -- стандартная запись использования аппликатива
Just 7
```
repl



grep `TODO` markers, fix it.
