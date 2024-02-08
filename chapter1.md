# FP Haskell, chapter 1, аппликативные функторы

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-1/test-applicative.hs)

Мотивация: на примере парсера текста, композиция вычислений с эффектами,
построение цепочек вычислений, создание комбинаторов (функций из функций).
Универсальные интерфейсы для тайп-классов.

definitions:
- Functor (fmap)
- Applicative (pure, `<*>`)
- Alternative (empty, `<|>`)
- Compose f g a, Applicative для Compose

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
    -- fmap = map -- для списка уже есть реализация fmap
    fmap _ [] = [] -- реализация руками (для потом, в траверсабл пригодится)
    fmap g (x:xs) = (:) (g x) (fmap g xs) -- префиксная запись рекурсивной реализации map

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

Если вспомнить, что pointfree это про топологию, точки и пространства, то pointed это "с точкой" или "в точку".
Т.е. значение это точка, Pointed.pure заворачивает точку в "контекст", который сам по себе тоже точка в некотором пространстве.

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

### 1.1.13 Applicative, pure law

Сочетание `pure` и `ap` (поднятие значения в контекст и поднятие вычисления в контекст) открывает большое количество разнообразных комбинаций.
Поэтому эти функции собрали в отдельном тайпклассе, `Applicative`.

Закон для `pure` в аппликативе.

Еще раз: Аппликатив (как наследник функтора) не меняет структуру контекста, не работает с эффектами, просто протаскивает их.
А вот монада уже может работать с эффектами.
```hs
import Control.Applicative -- где живет тайпкласс аппликатив

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

### 1.1.14 законы Applicative

Кроме закона, связывающего аппликатив, pointed и функтор: `fmap g x = pure g <*> x` ...

У аппликатива есть четыре закона (identity, homomorphism, interchange, composition):
первые три про свойства pure, четвертый про ассоциативность аппликации.

Реализуя инстансы тайпклассов (аппликатив), необходимо проверять и гарантировать выполнение этих законов.
Иначе получится не аппликатив а баг.
```hs
-- 1) identity law
(pure id) <*> v = v
-- id поднятая в контекст через pure, примененная через `applied over` к значению-в-контексте: не меняет это значение и контекст.
-- тривиальная природа pure

-- 2) homomorphism law
(pure g) <*> (pure x) = pure (g x)
-- заворачивание в контекст (функции, значения, результата) не влияет на аппликацию

-- 3) interchange law
fooFuncApplicative <*> pure x = pure ($ x) <*> fooFuncApplicative
-- ($) :: (a -> b) -> a -> b
-- что-то про коммутативность
-- неважно, что поднимать в pure: значение х или сечение оператора аппликации с х-ом
-- закон иллюстрирует отличие аппликатива от монады: в монаде порядок выполнения операций задан строго и не может быть изменен,
-- ибо эффекты не терпят коммутативности. В аппликативе эффекты не меняются, поэтому допустима некоторая коммутативность.
-- Монада может менять структуру контейнера, аппликатив: не может.

-- 4) composition law
pure (.) <*> u <*> v <*> x = u <*> (v <*> x)
-- как бы очевидно, оператор композиции, поднятый в контекст аппликатива, обязан обеспечить правую часть уравнения

-- examples

-- #2, homomorphism
ghci> pure length <*> pure "foo" :: Maybe Int -- уточним тип контекста
Just 3
ghci> pure $ length "foo" :: Maybe Int
Just 3

-- #3, interchange
ghci> (Just (^2) <*> pure 3) == (pure ($ 3) <*> Just (^2))
True

-- #4, composition
-- правая часть уравнения
ghci> Just (^2) <*> (Just succ <*> Just 3)
Just 16
ghci> (^2) $ succ 3
16
ghci> (^2) . succ $ 3
16
ghci> ((^2) . succ) 3
16
ghci> ((.) (^2)  succ) 3 -- а вот уже и левая часть уравнения проглядывает
16
-- левая часть уравнения
ghci> pure (.) <*> Just (^2) <*> Just succ <*> Just 3
Just 16
```
repl

```hs
https://stepik.org/lesson/28880/step/15?unit=9912
TODO
{--
Следующий тип данных задает гомогенную тройку элементов, которую можно рассматривать как трехмерный вектор

data Triple a = Tr a a a  deriving (Eq,Show)

Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой покоординатного применения

GHCi> (^2) <$> Tr 1 (-2) 3
Tr 1 4 9
GHCi> Tr (^2) (+2) (*3) <*> Tr 2 3 4
Tr 4 5 12
--}
data Triple a = Tr a a a deriving (Eq,Show)

-- solution

-- надо определить 2 инстанса - аппликатив и функтор

```
test

## chapter 1.2, Представители класса типов Applicative

https://stepik.org/lesson/30424/step/1?unit=11041

- Список как аппликативный функтор
- Список как аппликативный функтор: альтернативная семантика
- Either как представитель Applicative
- Пара как представитель Applicative
- Частично примененная функциональная стрелка как представитель Applicative
- Полное определение класса Applicative 
- Полное определение класса Applicative (продолжение)

### 1.2.2 аппликативный функтор списка

Реализация аппликативного функтора для списка.
Семантика: что значит "применить список функций к списку аргументов"?
1) Каждая ф. к каждому аргументу? Как в монаде списка
2) Попарное применение?
...

```hs
-- на примере: два списка, список функций и список значений
fs = [\x -> 2*x, \x -> 3+x, \x -> 4-x]
xs = [1, 2]
-- что должен делать аппликатив? Как в монаде списка, вложенный цикл (умножение количества)
ghci> fs <*> xs -- fs applied over xs
[2,4, 4,5, 3,2] -- для каждой функции: применить к каждому из аргументов
-- 2*1, 2*2; 3+1, 3+2; 4-1, 4-2

instance Applicative [] where
    pure x = [x]
    gs <*> xs = [g x | g <- gs, x <- xs] -- applied over
```
repl

```hs
https://stepik.org/lesson/30424/step/3?unit=11041
TODO
{--
Предположим, что для стандартного функтора списка оператор `(<*>)` определен стандартным образом,
а метод `pure` изменен

pure x = [x,x]

К каким законам класса типов `Applicative` будут в этом случае существовать контрпримеры?

Select all correct options from the list

- Applicative-Functor: g <$> xs ≡ pure g <*> xs
- Composition: (.) <$> us <*> vs <*> xs ≡ us <*> (vs <*> xs)
- Homomorphism: pure g <*> pure x ≡ pure (g x)
- Identity: pure id <*> xs ≡ xs
- Interchange: fs <*> pure x ≡ pure ($ x) <*> fs
--}

-- solution

https://pastebin.com/bUAyStqG
```
test

### 1.2.4 аппликативный функтор ZipList

Альтернативная семантика аппликатива для списка
```hs
-- стандартная семантика (как в монаде списка)
instance Applicative [] where
    pure x = [x]
    gs <*> xs = [g x | g <- gs, x <- xs] -- applied over

fs = [\x -> 2*x, \x -> 3+x, \x -> 4-x]
xs = [1, 2]
-- что должен делать аппликатив? Как в монаде списка, вложенный цикл (умножение количества)
ghci> fs <*> xs -- fs applied over xs
[2,4, 4,5, 3,2] -- для каждой функции: применить к каждому из аргументов
-- 2*1, 2*2; 3+1, 3+2; 4-1, 4-2

-- альтернатива: зипование, попарное применение

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Eq, Show)
-- не дозволено в Хаске переопределять существующие инстансы, поэтому пойдем через определение списка через newtype

instance Functor ZipList where -- аппликатив наследует функтор, поэтому сначала определим функтор
    fmap f (ZipList xs) = ZipList $ map f xs

instance Applicative ZipList where
    pure x = ZipList [x] -- invalid
    -- pure x = ZipList $ repeat x -- valid
    (ZipList gs) <*> (ZipList xs) = ZipList $ zipWith ($) gs xs -- применим стандартный zipWith

ghci> :i zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- Defined in ‘GHC.List’

ghci> ZipList fs <*> ZipList xs
ZipList {getZipList = [2,5]}
-- 2*1, 3+2

-- проверим выполнение закона №1, identity
ghci> (pure id <*> ZipList [1, 2]) == (ZipList [1, 2])
False
-- не работает. Почемк? Инвалидный pure, он дает список с одним элементом и последующий зип дает список с одним элементом.
-- а надо два (в примере)
pure x = ZipList [x] -- invalid

pure x = ZipList $ repeat x -- valid

ghci> :i repeat
repeat :: a -> [a]      -- Defined in ‘GHC.List’
```
repl

```hs
https://stepik.org/lesson/30424/step/5?unit=11041
TODO
{--
В модуле `Data.List` имеется семейство функций `zipWith`, `zipWith3`, `zipWith4`, ..

GHCi> let x1s = [1,2,3]
GHCi> let x2s = [4,5,6]
GHCi> let x3s = [7,8,9]
GHCi> let x4s = [10,11,12]
GHCi> zipWith (\a b -> 2*a+3*b) x1s x2s
[14,19,24]
GHCi> zipWith3 (\a b c -> 2*a+3*b+5*c) x1s x2s x3s
[49,59,69]
GHCi> zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
[9,15,21]

Аппликативные функторы могут заменить всё это семейство

GHCi> getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
[14,19,24]
GHCi> getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
[49,59,69]
GHCi> getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*>ZipList x3s <*> ZipList x4s
[9,15,21]

Реализуйте операторы `(>*<)` и `(>$<)`, позволяющие спрятать упаковку `ZipList` и распаковку `getZipList`

GHCi> (\a b -> 2*a+3*b) >$< x1s >*< x2s
[14,19,24]
GHCi> (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
[49,59,69]
GHCi> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
[9,15,21]
--}
import Control.Applicative (ZipList(ZipList), getZipList)

-- solution

```
test

### 1.2.6 Applicative Either

Продолжаем рассматривать реализации аппликативов
```hs
-- семантика: Left это эффект, ошибка. Аппликатив не должен делать эффекты, только пробрабывать.
-- e: error, Left: error, Right: value
instance Applicative (Either e) where
    pure = Right -- не могу сообразить, какие импликейшнс того, что в пюре мы не может положить ошибку Left?
    Left e <*> _ = Left e
    Right g <*> r = fmap g r -- where to look for Functor Either? fmap?

ghci> (*) <$> Right 6 <*> Right 7 -- <$> = fmap
Right 42

ghci> (*) <$> Left "foo" <*> Right 7
Left "foo"
ghci> (*) <$> Left "foo" <*> Left "bar"
Left "foo"

```
repl

### 1.2.7 аппликативный функтор пары

Продолжаем рассматривать реализации аппликативов
```hs
-- pair: (,) e a
-- e: environment, еще не стрелочный тип, но уже близко
-- один из двух параметров надо связать, первый (второй не получится)
-- семантика: эффект это запись в лог, операторы лог не создают но протаскивают
instance (Monoid e) => Applicative ((,) e) where
    pure x = (mempty, x) -- мемпти из моноида е
    (u, g) <*> (v, x) = (u `mappend` v, g x) -- складываем логи, apply g over x
-- без моноида было бы неясно, что делать с первым параметром

ghci> ("Answer to ", (*)) <*> ("the Ultimate ", 6) <*> ("Question", 7)
("Answer to the Ultimate Question",42)
```
repl

```hs
https://stepik.org/lesson/30424/step/8?unit=11041
TODO
{--
Функция

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

сворачивает список посредством деления. Модифицируйте ее, реализовав
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
такую что последовательность вычислений отражается в логе

GHCi> divideList [3,4,5]
3.75
GHCi> divideList' [3,4,5]
("<-3.0/<-4.0/<-5.0/1.0",3.75)

Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = _
divideList' (x:xs) = (/) <$> _ <*> _
--}
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = _
divideList' (x:xs) = (/) <$> _ <*> _

-- solution

```
test

### 1.2.9 аппликативный функтор стрелки

Продолжаем смотреть на реализации аппликативного функтора, стрелка.
Как результат, можно получить довольно загадочные выражения
```hs
-- аппликатив для стрелки e -> a
-- e: environment
instance Applicative ((->) e) where
    pure x e = x -- стрелка увеличивает арность, не забываем об этом
    pure x = \ e -> x -- через лямбду
    (<*>) :: f (a -> b) -> f a -> f b -- оригинальная сигнатура `applied over`
    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b) -- после подстановки
    (<*>) :: (e -> a -> b) -> (e -> a) -> e -> b -- убрали лишние скобки, увидели: откуда увеличение арности (доп. е)
    (<*>) g h e = g e (h e) -- реализация по сигнатуре: из е получаем а, из а получаем бе
    (<*>) eab ea e = eab e (ea e) -- так понятнее?
    (<*>) g h = \e -> g e (h e) -- через лямбду
-- e протаскивается во все вычисления
-- эффект: чтение из енв.

-- не забываем: окружение (2) протаскивается во все вычисления: (2 +) (2 * 3)
ghci> (+) <*> (*3) $ 2 -- читать такое вычисление надо справа-налево, ибо (g h = \e -> g e (h e))
8
-- (+) 2 ((*3) 2)
-- (*3) 2 -- это `(h e)`, первое вычисление
-- (+) 2 ... -- это `g e (...)`, второе вычисление
```
repl

```hs
https://stepik.org/lesson/30424/step/10?unit=11041
TODO
{--
Сделайте типы данных `Arr2 e1 e2` и `Arr3 e1 e2 e3` представителями класса типов `Applicative`

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

с естественной семантикой двух и трех окружений

GHCi> getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-1
GHCi> getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-15
--}
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap = undefined

instance Functor (Arr3 e1 e2 e3) where
  fmap = undefined

instance Applicative (Arr2 e1 e2) where
  pure = undefined
  (<*>) = undefined

instance Applicative (Arr3 e1 e2 e3) where
  pure = undefined
  (<*>) = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/30424/step/11?unit=11041
TODO
{--
Сопоставьте вычислению, поднятому в аппликативный функтор, 
конкретного представителя класса типов `Applicative`, в котором это вычисление происходит.

Match two lists

- pure zip <*> (Sum 5, [1,2,3]) <*> (Sum 4, [5,6])
- zip <*> tail
- \xs -> pure (++) <*> lookup 3 xs <*> lookup 5 xs
- (,) <$> "dog" <*> "cat"

- ?
- ?
- ?
- ?
--}

-- solution

pure zip <*> (Sum 5, [1,2,3]) <*> (Sum 4, [5,6])
-- здесь: 

zip <*> tail
-- здесь: 

\xs -> pure (++) <*> lookup 3 xs <*> lookup 5 xs
-- здесь: 

(,) <$> "dog" <*> "cat"
-- здесь: 

```
test

### 1.2.12 дополнительные методы `$>, *>`

Дополнительные функции в интерфейсе аппликативного функтора
(Control.Applicative, Data.Functor)
```hs
class Functor ...
    (<$) :: a -> f b -> f a
    ($>) :: f a -> b -> f b -- структура (контекст) из первого параметра, значение из второго параметра

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (<*) :: f a -> f b -> f a
    (*>) :: f a -> f b -> f b -- похоже на flip const; значение берем из второго параметра, эффекты протягиваем из первого и второго

-- (*>) :: f a -> f b -> f b
ghci> [1,2,3] *> [4,5]
[4,5, 4,5, 4,5]
-- структура соответствует семантике `apply over` для списков (вложенный цикл),
-- но значения только из второго параметра
```
repl

### 1.2.13 дополнительные методы `<$, <*, liftA, <**>`

Продолжим про дополнительные функции
```hs
class Functor ...
    ($>) :: f a -> b -> f b -- структура (контекст) из первого параметра, значение из второго параметра
    (<$) :: a -> f b -> f a -- наоборот, структура из второго, значение из первого

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>) :: f a -> f b -> f b -- похоже на flip const; значение берем из второго параметра, эффекты протягиваем из первого и второго
    (<*) :: f a -> f b -> f a -- аналогично, эффекты полностью, значения из первого параметра

-- (<*) :: f a -> f b -> f a
ghci> [1,2,3] <* [4,5]
[1,1, 2,2, 3,3]
-- аналогично правой стрелке, структура отражает семантику applied over для списков (вложенный цикл)
-- а значения берет из левого параметра (куда показывает уголок)

-- реализация по умолчанию (в классе)
(<*) :: f a -> f b -> f a -- эффекты полностью, значения из первого параметра
u (<*) v = (pure const) <*> u <*> v

(*>) :: f a -> f b -> f b -- похоже на flip const; значение берем из второго параметра, эффекты протягиваем из первого и второго
u (*>) v = (pure $ flip const) <*> u <*> v

-- лифты: поднятие функции в контекст

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = (pure f) <*> a
liftA f a = fmap f a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b -- fmap2

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c -- fmap3

-- ап две звезды, разворачивание пайплайна

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
-- apply over, но с переставленными аргументами
-- последовательность эффектов сохраняется, но цепочка вычислений композится в обратном направлении,
-- удобно читать цепочку в такой записи как "пайплайн" вычислений

-- казалось бы, почему не записать так:
(<**>) = flip (<*>) -- но это совсем другая реализация, это нельзя читать как соединение частий пайплайна

-- пример: вот была такая цепочка на аппликативе стрелки
ghci> (+) <*> (*3) $ 2 -- цепочка вычислений (2 +) (2 *3)
8
ghci> (*3) <**> (+) $ 2 -- цепочка развернулась: (2 *3) (+ 2)
8
```
repl

```hs
https://stepik.org/lesson/30424/step/14?unit=11041
TODO
{--
Двойственный оператор аппликации `(<**>)` из модуля `Control.Applicative`
изменяет направление вычислений, не меняя порядок эффектов

infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

Определим оператор `(<*?>)` с той же сигнатурой, что и у `(<**>)`, но другой реализацией

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

Для каких стандартных представителей класса типов `Applicative` 
можно привести цепочку аппликативных вычислений, 
дающую разный результат в зависимости от того, какой из этих операторов использовался?

В следующих шести примерах вашей задачей будет привести такие контрпримеры для стандартных типов данных, для которых они существуют.

Следует заменить аппликативное выражение в предложении `in`
на выражение того же типа, однако дающее разные результаты
при вызовах с 
`(<??>) = (<**>)` 
и 
`(<??>) = (<*?>)`

Проверки имеют вид `exprXXX (<**>) == exprXXX (<*?>)`
для различных имеющихся `XXX`

Если вы считаете, что контрпримера не существует, то менять ничего не надо
--}
{-# LANGUAGE RankNTypes #-}
import Control.Applicative ((<**>), ZipList(..))

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Just 5 <??> Just (+2) -- place for counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in [1,2] <??> [(+3),(+4)] -- place for counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- place for counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Left "AA" <??> Right (+1)  -- place for counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ("AA", 3) <??> ("",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op = 
  let (<??>) = op 
      infixl 4 <??> 
  in length <??> (\_ -> (+5))  -- place for counterexample

-- solution

-- нужно оставить оператор <??>
-- вместо него грейдер будет подставлять <**>, <*?> и сравнивать результаты.
-- Вам нужно лишь подобрать такие операнды, при которых результаты "вызова" будут различаться

```
test

## chapter 1.3, Аппликативный парсер Parsec

https://stepik.org/lesson/42245/step/1?unit=20509

- Parsec
- Парсер комбинаторы
- Аппликативный интерфейс Parsec

### 1.3.2 Parsec, библиотека парсер-комбинаторов

Хороший пример сервиса с аппликативным интерфейсом: парсер (синтаксический разборщик).
На входе поток символов, на выходе некотарая структура, в соответствии с грамматикой.
Или ошибка.
Рассмотрим существующий парсер (либа парсер-комбинаторов).
Сложные парсеры создаются из простых, есть готовый набор простых и тривиальных комбинаторов из этих простых.
```hs
import Text.Parsec
import Control.Applicative ((*>), (<*))

-- парсер `digit` возвращает символ-цифру или ошибку
parseTest digit "12ab"
'1'
parseTest digit "ab21"
error: unexpected "a"

-- парсер `letter`
parseTest letter "12ab"
error
parseTest letter "ab21"
'a'

parseTest -- это функция для вывода результата работы парсера на консоль (Monad IO)
ghci> :i parseTest
parseTest ::
  (Stream s Data.Functor.Identity.Identity t, GHC.Show.Show a) =>
  Parsec s () a -> s -> GHC.Types.IO () -- Defined in ‘Text.Parsec.Prim’
-- разберем сигнатуру
Parsec s () a -- первый параметр, парсер из потока s в некий a, стейт в виде юнита () игнорится
s -- второй параметр, входная строка
IO () -- выход, что-то напечатали на консоль

-- есть функция для программной обработки результатов работы парсера, `parse`
ghci> parse letter "" "ab21"
Right 'a'

ghci> parse letter "" "12ab"
Left (line 1, column 1): unexpected "1" expecting letter

ghci> :i parse
parse ::
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s () a -> SourceName -> s -> Data.Either.Either ParseError a -- Defined in ‘Text.Parsec.Prim’
-- разберем сигнатуру
Parsec s () a -- первый параметр, парсер из потока s в некий a, стейт в виде юнита () игнорится
SourceName -- второй параметр, характеристика источника для трейсинга ошибок
s -- третий параметр, входная строка
Either ParseError a -- выход, ошибка или результат a

-- функция создания парсера: `oneOf`
vowel :: Parsec [Char] u Char
vowel = oneOf "aeiou"

ghci> parseTest vowel "abc"
'a'
ghci> parseTest vowel "bcd"
parse error at (line 1, column 1): unexpected "b"

Parsec -- трех-параметрический конструктор типов
Parsec [Char] u Char
[Char] -- тип входного потока
u -- определяемое пользователем состояние, поддержка стейта парсера
Char -- тип выходного значения

-- n.b. все параметризовано, можно парсить что угодно
-- на входе могут быть токены (после лексера) или другие типы
```
repl

```hs
ghci> :browse Text.Parsec.Char
alphaNum ::
  Stream s m GHC.Types.Char => ParsecT s u m GHC.Types.Char
anyChar ::
  Stream s m GHC.Types.Char => ParsecT s u m GHC.Types.Char
char ::
  Stream s m GHC.Types.Char =>
  GHC.Types.Char -> ParsecT s u m GHC.Types.Char
...
```
extra

```hs
https://stepik.org/lesson/42245/step/3?unit=20509
TODO
{--
Какие из следующих примитивных парсеров имеются в библиотеке `Text.Parsec.Char`

Select all correct options from the list

- Парсер, разбирающий в точности символ новой строки ('\n')
- Парсер, разбирающий в точности символ пробела (' ')
- Парсер, разбирающий в точности последовательность символов возврата каретки ('\r') и новой строки ('\n')
- Парсер, разбирающий произвольный символ
- Парсер, разбирающий в точности символ возврата каретки ('\r')
- Парсер, разбирающий в точности символ табуляции ('\t')
--}

-- solution

```
test

### 1.3.4 парсер-комбинаторы Parsec (many, count, endBy)

В предыдущей главе посмотрели на примитивные парсеры.
Теперь посмотрим на комбинаторы.
```hs
-- many1: один или большее число раз
ghci> parseTest (many1 digit) "123abc"
"123"
ghci> parseTest (many1 digit) "d123abc"
parse error at (line 1, column 1): unexpected "d" expecting digit

ghci> :i many1
many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a] -- Defined in ‘Text.Parsec.Combinator’
-- один или большее число раз: применяет парсер из параметра, до получения ошибки
-- трансформация парсера-выдающего-одно-значение в парсер-выдающий-список

-- для случая 0 или более раз: парсер-комбинатор `many`
ghci> parseTest (many digit) "d123abc"
"" -- 0 успешных результатов это тоже успех для данного комбинатора
ghci> parseTest (many digit) "123abc"
"123"

-- count: строго заданное количество раз успешно отработавший парсер из параметра
ghci> parseTest (count 3 digit) "123abc"
"123"
ghci> parseTest (count 2 digit) "123abc"
"12"
ghci> parseTest (count 4 digit) "123abc"
parse error at (line 1, column 4): unexpected "a" expecting digit

ghci> :i count
count ::
  Stream s m t =>
  GHC.Types.Int -> ParsecT s u m a -> ParsecT s u m [a] -- Defined in ‘Text.Parsec.Combinator’

-- endBy: первый парсер, после которого выполняется второй парсер
ghci> parseTest (count 3 digit `endBy` (char 'b')) "123bc"
["123"]
ghci> parseTest (count 3 digit `endBy` (char 'b')) "123b456b"
["123","456"]
ghci> parseTest (count 3 digit `endBy` (char 'b')) "123abc"
parse error at (line 1, column 4): unexpected "a" expecting "b"

ghci> :i endBy
endBy ::
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a] -- Defined in ‘Text.Parsec.Combinator’
```
repl

```hs
https://stepik.org/lesson/42245/step/5?unit=20509
TODO
{--
Реализуйте парсер `getList`, который разбирает строки из чисел, разделенных точкой с запятой,
и возвращает список строк, представляющих собой эти числа:

GHCi> parseTest getList "1;234;56"
["1","234","56"]

GHCi> parseTest getList "1;234;56;"
parse error at (line 1, column 10):
unexpected end of input
expecting digit

GHCi> parseTest getList "1;;234;56"
parse error at (line 1, column 3):
unexpected ";"
expecting digit

Совет: изучите парсер-комбинаторы, доступные в модуле `Text.Parsec`, и постарайтесь найти наиболее компактное решение.
--}
import Text.Parsec
getList :: Parsec String u [String]
getList = undefined

-- solution
https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Combinator.html

```
test

### 1.3.6 семантика аппликатива Parsec (`pure, <*>`)

Семантика аппликативного функтора для Parsec.
Что значит `pure`, `applied over`?

`pure` игнорирует первые параметры конструктора Parsec и возвращает переданное значение, завернутое в конструктор Parsec.

`applied over` соединяет два парсера, где первый откусывает от входа свою часть, второй работает уже с хвостом входа;
результаты обоих обрабатывается поднятой-в-аппликатив-функцией.
Если где-то в цепочке вылезла ошибка, весь пайплайн выдает эту ошибку.
```hs
-- pure: посмотрим на него
ghci> import Control.Applicative
ghci> parseTest (pure 42) "foo"
42
-- что завернули в Parsec, то и вернули: 42

-- applied over

-- возьмем пример пайплайна, построенного на аппликативе
-- парсер выдает пару из собранных в цепочку парсеров
p0 :: Parsec [Char] u ([Char], [Char])
p0 = pure (,) <*> many1 letter <*> many1 digit
p0 = (,) <$> many1 letter <*> many1 digit -- более традиционная запись, конструктор пары fmap парсер буков ...
-- слева-направо: функция двух аргументов (конструктор пары) поднимается в аппликатив,
-- первым аргументом для пары будет 1.. буков, вторым аргументом будет 1.. цифр

ghci> parseTest p0 "abc123"
("abc","123")

-- пример пайплайна, первый результат игнорируется
ghci> import Text.Parsec as TP
ghci> parseTest (string "abc" *> TP.many digit) "abc123"
"123"
-- комбинация через правую ап, парсит первый парсер, игнорит его, парсит второй.

-- чуть сложнее, пара из первого парсера и цепочки из второго-и-третьего
-- результат второго игнорируется
p1 :: Parsec [Char] u ([Char], [Char])
p1 = (,) <$> many1 letter <*> (many space *> many1 digit)
 -- игнорим пробелы 0..
ghci> parseTest p1 "abc123"
("abc","123")
ghci> parseTest p1 "abc 123"
("abc","123")

-- тот же эффект, другая запись (левая ассоциативность тут играет)
p1 = (,) <$> many1 letter <* many space <*> many1 digit
-- 1.. буков и следом 0.. пробелов с игнором второго результата. Второй элемент пары 1.. цифр

ghci> parseTest p1 "abc  123"
("abc","123")
```
repl

```hs
https://stepik.org/lesson/42245/step/7?unit=20509
TODO
{--
Используя аппликативный интерфейс `Parsec`, реализуйте функцию `ignoreBraces`
которая принимает три аргумента-парсера. 
Первый парсер разбирает текст, интерпретируемый как открывающая скобка, 
второй — как закрывающая, 
а третий разбирает весь входной поток, расположенный между этими скобками
Возвращаемый парсер возвращает результат работы третьего парсера, скобки игнорируются

GHCi> test = ignoreBraces (string "[[") (string "]]") (many1 letter)
GHCi> parseTest test "[[ABC]]DEF"
"ABC"
--}
import Text.Parsec
ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces = undefined

-- solution

```
test

## chapter 1.4, Аппликативный парсер своими руками

https://stepik.org/lesson/30425/step/1?unit=11042

- Выбор типа для парсера
- Интерфейс функтора для парсера 
- Аппликативный интерфейс для парсера
- Реализация примитивных парсеров
- Класс типов Alternative
- Парсер с выбором альтернатив
- Рекурсия при синтаксическом разборе

### 1.4.2 тип парсера Parser

Начинаем сооружать парсер с аппликативным интерфейсом.
Проведем некоторые рассуждения на тему, какой тип нам нужен, исходя из желаемой функциональности.
```hs
import Data.Char
import Control.Applicative hiding (many)

-- тип парсера: строка в произвольную структуру
type Parser a = String -> a
-- стрелочный тип (аппликатив: композиция функций)

-- такой парсер уже есть:
ghci> :i read
read :: GHC.Read.Read a => GHC.Base.String -> a -- Defined in ‘Text.Read’

ghci> read "42" :: Int
42
-- есть проблема: что-то сложнее примитивов так распарсить не получается
ghci> read "14*3" :: Int
*** Exception: Prelude.read: no parse

-- надо поддержать более сложные варианты, как?
-- нужна поддержка состояния, стейта, где будет лежать еще не распарсенный хвост ввода
type Parser a = String -> (a, String)

-- теперь надо подумать о возможных ошибках, если парсер не может распарсить вход
type Parser a = String -> Maybe (a, String)

-- опция Maybe не дает возможности показывать осмысленные ошибки,
-- Either дает
type Parser a = String -> Either String (a, String)

-- далее: а как быть в случаях, когда разбор можно выполнить несколькими способами?
-- если валидных вариантов выхода из парсера 1..
-- неоднозначные грамматики
type Parser a = String -> [(a, String)]
-- пустой список: не удалось распарсить (текст ошибки потеряли)
-- 1.. варианты годного результата

-- такой парсер тоже есть
ghci> :i reads
reads :: Read a => ReadS a      -- Defined in ‘Text.Read’
ghci> :i ReadS
type ReadS :: * -> *
type ReadS a = String -> [(a, String)] -- Defined in ‘Text.ParserCombinators.ReadP’

ghci> reads "14*3" :: [(Int,String)]
[(14,"*3")]

-- увы, тут тоже не без проблем
ghci> reads "*3" :: [(Int,String)]
[]
-- облом парсинга числа из звездочки ломает потенциальный пайплайн
-- хотелось бы некие комбинаторы ...
-- для этого нужно аппликативный интерфейс (чтобы соединять парсеры в цепочки)
-- для этого нужно правильно задать тип

-- using `data` ... or
newtype Parser a = Parser { apply :: String -> [(a, String)] }
-- but not `type`

ghci> :t apply
apply :: Parser a -> String -> [(a, String)] -- взять парсер, строку, вернуть список пар (значение, остаток-входа)

-- вспомогательная функция
parse :: Parser a -> String -> a -- первый результат парсинга
parse p = (fst . head . apply) p
```
repl

### 1.4.3 Functor Parser, anyChar

Реализовать интерфейсы функтора и аппликатива.
Построить набор "кирпичиков": примитивных парсеров, (пригодных для комбинирования ибо есть аппликативный интерфейс).
```hs
import Data.Char
import Control.Applicative hiding (many)

newtype Parser a = Parser { apply :: String -> [(a, String)] }

anyChar :: Parser Char -- примитивный парсер №1
anyChar = Parser f where -- парсер это стрелка из строки в список пар; пара содержит распарсенное значение и остаток строки
    f "" = []
    f (c:cs) = [(c, cs)]

ghci> apply anyChar "abc"
[('a',"bc")]

-- реализуем интерфейс функтора
instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b -- fmap or <$>
    fmap :: (a -> b) -> Parser a -> Parser b -- fmap or <$>
    fmap f p = Parser fun where -- парсер `p` это функция (ака стрелка); `apply p` дает нам функцию из строки в список
        fun s = [ (f a, s2) | (a, s2) <- apply p s ]
-- что значит "поднять функцию в функтор" в контексте парсера?
-- это значит: применить данную функцию ко всем значениям (голова пары) из списка,
-- полученного применением данного парсера к той строке, на которой это будет работать (параметр `s`)

-- пример функтора, который позволил нам создать (скомбинировать) новый парсер `digitToInt <$> anyChar`

ghci> apply (digitToInt <$> anyChar) "123abc"
[(1,"23abc")]
ghci> :t apply (digitToInt <$> anyChar) "123abc"
apply (digitToInt <$> anyChar) "123abc"
  :: [(Int, String)]

-- digitToInt не тотальная функция
ghci> apply (digitToInt <$> anyChar) "quix"
*** Exception: Char.digitToInt: not a digit 'q'
```
repl

```hs
https://stepik.org/lesson/30425/step/4?unit=11042
TODO
{--
Предположим, тип парсера определен следующим образом:

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

Сделайте этот парсер представителем класса типов `Functor`

Реализуйте также парсер `anyChr :: Prs Char`
удачно разбирающий и возвращающий любой первый символ любой непустой входной строки

GHCi> runPrs anyChr "ABC"
Just ('A',"BC")
GHCi> runPrs anyChr ""
Nothing
GHCi> runPrs (digitToInt <$> anyChr) "BCD"
Just (11,"CD")
--}
instance Functor Prs where
  fmap = undefined
anyChr :: Prs Char
anyChr = undefined

-- solution

```
test

### 1.4.5 Applicative Parser (pure, applied over)

Семантика аппликатива для парсера.
- `pure`: ничего не делать, упаковать переданное значение в структуру (в пару).
- `<*>` aka `applied over`: слева "парсер" функций, справа "парсер" значений (голова пары, слева ф. и справа некий х).
К входной строке применим пераый парсер, на полученном хвосте строки применим второй парсер,
применим ф. из первого парсера к результату из второго.

```hs
newtype Parser a = Parser { apply :: String -> [(a, String)] }
instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b -- fmap or <$>
    fmap :: (a -> b) -> Parser a -> Parser b -- fmap or <$>
    fmap f p = Parser fun where -- парсер `p` это функция (ака стрелка); `apply p` дает нам функцию из строки в список
        fun s = [ (f a, s2) | (a, s2) <- apply p s ]

-- реализуем аппликативный интерфейс

instance Applicative Parser where
    -- pure :: a -> f a -- aka: singleton, return, unit, point
    pure :: a -> Parser a
    pure a = Parser fun where
        fun s = [(a, s)] -- просто упаковать переданные данные в структуру парсера

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pv = Parser fun where
        fun s = [ (g a, s3) | (g, s2) <- apply pf s, (a, s3) <- apply pv s2 ]
-- `(g, s2) <- apply pf s` -- наружный цикл, результаты работы левого парсера (парсера где функции)
-- `(a, s3) apply pv s2` -- внутренний цикл, результаты правого парсера на хвостах от левого
-- `(g a, s3)` -- комбинация, функция из левого применяется к результату из правого, хвост из правого

-- пример цепочки парсеров, собранных в новый (комбинированный) парсер

ghci> apply ((,) <$> anyChar <*> anyChar) "abcde"
[(('a','b'),"cde")]
-- `(,) <$> anyChar` -- это fmap, создает парсер со значениями-функциями, для подстановки в первый параметр `applied over`
-- конструктор пары, где первый элемент из первого парсера, второй из второго
ghci> apply (pure (,) <*> anyChar <*> anyChar) "abcde" -- три парсера в цепочке аппликатива
[(('a','b'),"cde")]

-- examples: два парсера в цепочке

ghci> apply (anyChar *> anyChar) "abcde" -- результат из второго парсера, первый работает но значение игнорится
[('b',"cde")]
-- и наоборот
ghci> apply (anyChar <* anyChar) "abcde"
[('a',"cde")]
```
repl

```hs
https://stepik.org/lesson/30425/step/6?unit=11042
TODO
{--
Сделайте парсер из предыдущей задачи

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

аппликативным функтором с естественной для парсера семантикой:

GHCi> runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
Just (('A','B','C'),"DE")
GHCi> runPrs (anyChr *> anyChr) "ABCDE"
Just ('B',"CDE")

Представитель для класса типов Functor уже реализован
--}
instance Applicative Prs where
  pure  = undefined
  (<*>) = undefined

-- solution

```
test

### 1.4.7 Parser lib (satisfy, lower, char, digit, multiplication)

Реализуем набор стандартных примитивных парсеров и комбинаторов
```hs
-- функция создания парсера из предиката
satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
    f ""                    = []
    f (c:cs)    | pr c      = [(c, cs)]
                | otherwise = []

lower :: Parser Char
lower = satisfy isLower

ghci> apply lower "abc"
[('a',"bc")]
ghci> apply lower "Abc"
[]

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

ghci> apply (char 'B') "abc"
[]
ghci> apply (char 'B') "Babc"
[('B',"abc")]

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit -- умножить две цифры, разделенные *, сама звезда игнор.

ghci> apply multiplication "2*5"
[(10,"")]
```
repl

```hs
https://stepik.org/lesson/30425/step/8?unit=11042
TODO
{--
Рассмотрим более продвинутый парсер, позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

Реализуйте функцию `satisfyE`

satisfyE :: (Char -> Bool) -> PrsE Char 

таким образом, чтобы функция `charE`

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

обладала бы следующим поведением:

GHCi> runPrsE (charE 'A') "ABC"
Right ('A',"BC")
GHCi> runPrsE (charE 'A') "BCD"
Left "unexpected B"
GHCi> runPrsE (charE 'A') ""
Left "unexpected end of input"
--}
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE = undefined

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

-- solution

```
test

```hs
https://stepik.org/lesson/30425/step/9?unit=11042
TODO
{--
Сделайте парсер из предыдущей задачи

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

функтором и аппликативным функтором:

GHCi> let anyE = satisfyE (const True)
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
Right (('A','C'),"DE")
GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
Left "unexpected B"
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
Left "unexpected end of input"
--}
instance Functor PrsE where
  fmap = undefined

instance Applicative PrsE where
  pure  = undefined
  (<*>) = undefined

-- solution

```
test

### 1.4.10 type-class Alternative

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Applicative.html#g:2

Альтернатив это производная от Аппликатив, его расширение.
Добавляет к аппликативному функтору некую моноидальную структуру (нейтраль и суммирование).

В некотором роде, альтернатив похож на моноид суммы,
если сравнивать аппликатив с моноидом произведения.

Моноид не подходит по причине несовпадения требуемых kind.
Альтернатив это тайп-класс более высокого кайнда.
```hs
-- освежим память, что такое моноид

class Monoid a where
    mempty :: a -- нейтраль
    mappend :: a -> a -> a -- ассоциативная операция

-- тип "а" имеет значение, должен быть моноидом
instance (Monoid a) => Monoid (Maybe a) where -- если а моноид, можно сделать моноидальный мэйби
    mempty = Nothing
    Nothing `mappend` m = m 
    m `mappend` Nothing = m
    (Just m1) `mappend` (Just m2) = Just (m1 `mappend` m2)
-- реализация прячет Nothing и склеивает (моноид) значения

-- альтернативная реализация, прокидывает первое (левое) не-пустое значение, склейка невозможна (нет контекста моноид)
-- альтернативная реализация упакована в ньютайп, во избежание конфликта имен
newtype First a = First { getFirst :: Maybe a }
-- по сути это вот это:
-- тип "а" не важен, двигаем на выход первый не-пустой
instance Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` _ = m

-- посмотрим на Alternative: A monoid on applicative functors

-- у него другой (отличный от моноида) kind, ему нужен конструктор типов;
-- у него больше законов.

-- моноидальные свойства контекста
infixl 3 <|> -- более низкий приоритет (от <*>, <$>, ...)
class (Applicative f) => Alternative f where -- расширим аппликатив
    empty :: f a -- нейтраль, похоже на моноидальный mempty, но это "нейтральный контекст", без конкретного значения
    (<|>) :: f a -> f a -> f a -- ассоциативная операция, сигнатура как моноид mappend, не так ли? нет, но похоже. Здесь это "а в контексте".
-- empty: the identity of `<|>`
-- <|>: associative binary operation

instance Alternative [] where -- реализация альтернативы для списка
    empty = []
    (<|>) = (++) -- список позволяет комбинировать: склеить два списка
-- альтернатива для списка повторяет поведение моноида для списка

ghci> "abc" <|> "def"
"abcdef" -- 3+3 = 6
-- для списка альтернатива <|> работает как "сложение" sum
-- в то время как аппликатив <*> работает как "умножение" product
ghci> const <$> "abc" <*> "def"
"aaabbbccc" -- 3*3 = 9

-- альтернатива для мейби это вот та самая другая реализация моноида: из цепочки выходит первый не-пустой
instance Alternative Maybe where -- наложить ограничение на `a` мы не можем, класс не позволяет добавить параметр `a`
    empty = Nothing
    Nothing <|> r = r
    l <|> _ = l
-- без ограничений на `a` это единственная разумная реализация (First)
-- первый не-пустой, органично ложится на тип-суммы из двух конструкторов
ghci> Nothing <|> Just 3 <|> Just 5 <|> Nothing
Just 3

-- Законы Alternative

--Для Alternative, к трем законам моноида:
mempty `mappend` x          = x                             -- #1 left empty
x `mappend` mempty          = x                             -- #2 right empty
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)   -- #3 append associativity
--добавляются еще четыре:

-- №1 правая дистрибутивность ap <*>
(f <|> g) <*> a = (f <*> a) <|> (g <*> a) -- like (f + g) * a
-- №2 right absorbtion ap <*>
empty <*> a = empty -- like 0 * x
-- #1, #2 описывают связь между аппликативом и альтернативом

-- №3, №4 описывают связь между альтернативом и функтором
-- #3 left distributivity of fmap
f <$> (a <|> b) = (f <$> a) <|> (f <$> b)
-- #4 left absorbtion for fmap
f <$> empty = empty
```
repl

### 1.4.11 Alternative Parser

Почему альтернатив это не моноид?

> `MonoidK` — это моноид для конструкторов типов вида `* -> *`, без всяких ограничений на applicative, т.е. alternative без applicative.
- https://typelevel.org/cats/typeclasses/monoidk.html
- https://typelevel.org/cats/typeclasses/semigroupk.html

Резюмируя:
- у моноида сложение зависит от типа, хранящегося в контейнере
- у MonoidK сложение не зависит от типа в контейнере

Семантика applied over `<*>` в парсере: применить левый парсер к входной строке, на хвосте применить правый парсер;
левую функцию применить к результату правого парсера.

Хотим в наш Parser добавить семантику "альтернативы".
Семантика alternative `<|>` в парсере: применить левый, если успешно, то это и есть результат;
иначе применить правый и вернуть его результат.

```hs
instance Alternative Parser where
    empty :: Parser a
    empty = Parser f where -- парсер это функция, стрелка
        f _ = [] -- результат работы парсера это список пар
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser f where -- левый или правый-если-левый-сломан
        f s = let ps = apply p s in -- применим левый парсер
        if null ps -- и проверим результат
            then apply q s
            else ps

-- хотим получить А или В
ghci> apply (char 'A' <|> char 'B') "ABC"
[('A',"BC")]
ghci> apply (char 'A' <|> char 'B') "BC"
[('B',"C")]
ghci> apply (char 'A' <|> char 'B') "C"
[]

```
repl

morse
```hs
GHCi> parse (some $ morse) "..-"
["EET","EA","IT","U"]

morse :: Parser Char
morse = Parser f1 <|> Parser f2 <|> Parser f3 where
    f1 ('.':cs) = [('E',cs)]
    f1 ('-':cs) = [('T',cs)]
    f1 _ = []

    f2 ('.':'-':cs) = [('A',cs)]
    f2 ('.':'.':cs) = [('I',cs)]
    f2 ('-':'.':cs) = [('N',cs)]
    f2 ('-':'-':cs) = [('M',cs)]
    f2 _ = []

    f3 ('.':'.':'-':cs) = [('U',cs)]
    -- etc
    f3 _ = []
```
extra

```hs
https://stepik.org/lesson/30425/step/12?unit=11042
TODO
{--
Сделайте парсер `Prs` представителем класса типов `Alternative`
с естественной для парсера семантикой

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

GHCi> runPrs (char 'A' <|> char 'B') "ABC"
Just ('A',"BC")
GHCi> runPrs (char 'A' <|> char 'B') "BCD"
Just ('B',"CD")
GHCi> runPrs (char 'A' <|> char 'B') "CDE"
Nothing

Представители для классов типов `Functor` и `Applicative` уже реализованы.
Функцию 
char :: Char -> Prs Char
включать в решение не нужно, но полезно реализовать для локального тестирования.
--}
instance Alternative Prs where
  empty = undefined
  (<|>) = undefined

-- solution
import Control.Applicative

```
test

### 1.4.13 parser lowers, many

Класс парсеров: переваривать голову строки (списка) в цикле. `0..` буков как результат парсинга.
Полезно для пропуска пробельных символов.

Создадим парсер выборки символов в нижнем регистре.
```hs
-- первая попытка, рекурсивное использование парсера одной буквы
lowers :: Parser String
lowers = (pure (:)) <*> lower <*> lowers -- рекурсивное построение списка

ghci> apply lower "abc"
[('a',"bc")]
ghci> apply lower "Abc"
[]

-- увы, не работает. Почему? Аппликатив убивает весь результат при встрече неудачи
ghci> apply lowers "abCdef"
[]
ghci> apply lowers "abcdef"
[]

-- вторая попытка

-- добавим альтернативу, которая вместо обнуления всего при неудаче,
-- даст удачное терминирование рекурсии с сохранением накопленного результата
lowers = (pure (:)) <*> lower <*> lowers <|> (pure "")
-- работает как ожидаем
ghci> apply lowers "abcdef"
[("abcdef","")]
ghci> apply lowers "abCdef"
[("ab","Cdef")]
ghci> apply lowers "AbCdef"
[("","AbCdef")]

-- обобщение паттерна

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> (many p) <|> (pure [])
-- n.b. аппликативный функтор и моноидальный альтернатив используются без привязки к типу значения,
-- how cool is that!

ghci> apply (many digit) "123abc"
[([1,2,3],"abc")]

-- парсер many всегда успешен, неудач у него не бывает.
ghci> apply (many digit) "abc"
[([],"abc")] -- удачное завершение парсера, с пустым результатом
ghci> apply (digit) "abc"
[] -- неудачное завершение парсера

```
repl

```hs
https://stepik.org/lesson/30425/step/14?unit=11042
TODO
{--
Реализуйте для парсера `Prs` парсер-комбинатор `many1`

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
many1 :: Prs a -> Prs [a]

который отличается от `many` только тем, что он терпит неудачу в случае,
когда парсер-аргумент неудачен на начале входной строки

> runPrs (many1 $ char 'A') "AAABCDE"
Just ("AAA","BCDE")
> runPrs (many1 $ char 'A') "BCDE"
Nothing

Функцию `char :: Char -> Prs Char` включать в решение не нужно, но полезно реализовать для локального тестирования
--}
many1 :: Prs a -> Prs [a]
many1 = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/30425/step/15?unit=11042
TODO
{--
nat :: Prs Int
mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

Реализуйте парсер `nat` для натуральных чисел,
так чтобы парсер `mult` обладал таким поведением

GHCi> runPrs mult "14*3"
Just (42,"")
GHCi> runPrs mult "64*32"
Just (2048,"")
GHCi> runPrs mult "77*0"
Just (0,"")
GHCi> runPrs mult "2*77AAA"
Just (154,"AAA")

Реализацию функции 
char :: Char -> Prs Char 
следует включить в присылаемое решение, только если она нужна для реализации парсера `nat`
--}
nat :: Prs Int
nat = undefined

-- solution
-- от студента нужен только работающий nat, а mult и char мы в тестирующей части используем свои

```
test

## chapter 1.5, Композиция на уровне типов

https://stepik.org/lesson/30426/step/1?unit=11043

- Оператор композиции для типов
- Композиция функторов как функтор
- Выполнение законов функтора для композиции
- Композиция как аппликативный функтор: pure
- Композиция как аппликативный функтор: семантика (<*>)
- Композиция как аппликативный функтор: реализация (<*>)

### 1.5.2 Data.Functor.Compose

`|.|` aka `Compose` (`Data.Functor.Compose`)
композиция двух типов (функторов), чтобы получить функтор.

Пример.
Если матрица это список списков, то матрица это композиция двух типов?
Чтобы матрица-функтор могла применить функцию (скажем, `^2`) к элементам (`fmap`),
надо эту функцию протащить через два "барьера": список списков.

Идею можно обобщить, имея два функтора, сделать их композицию для такого протаскивания функций ...
получим универсальный `fmap` для композиции функторов.

Чтобы это сделать на уровне типов, нужен тип
```hs
{-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
ghci> :set -XTypeOperator
{-# LANGUAGE PolyKinds #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show) -- design time wrapper
-- `f (g a)` это два конструктора типов, примененные к а.
-- kind a = *
-- kind g = * -> *
-- kind f = * -> *
ghci> :kind (|.|)
(|.|) :: (k -> *) -> (k1 -> k) -> k1 -> * -- (* -> *) -> (* -> *) -> * -> *

ghci> :t Just "abc"
Just "abc" :: Maybe [Char]
-- конструктор списка, сверху конструктор Maybe
ghci> :t Just "abc" :: Maybe ([] Char)
Just "abc" :: Maybe ([] Char) :: Maybe [Char]
-- с помощью нашего оператора композиции типов:
ghci> :t Cmps (Just "abc")
Cmps (Just "abc") :: (|.|) Maybe [] Char
-- та же жопа только в профиль
ghci> :t Cmps (Just "abc") :: (Maybe |.| []) Char -- `Maybe |.| []` композиция двух типов (функторов)
Cmps (Just "abc") :: (Maybe |.| []) Char :: (|.|) Maybe [] Char
```
repl

```hs
https://stepik.org/lesson/30426/step/3?unit=11043
TODO
{--
Населите допустимыми нерасходящимися выражениями следующие типы 

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer
--}
type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = undefined

b :: B t
b = undefined

c :: C
c  = undefined

-- solution
-- haskell hole driven development
-- Вместо undefined пишем: Cmps (_ a)

```
test

### 1.5.4 релизация fmap для Compose

Оператор композиции типов очень похож на оператор композиции функций.
По сути, конструктор типа это и есть функция, почему бы такие функции не композить.

Хотим сделать оператор `Compose` функтором. Значит, надо связать все параметры,
кроме последнего
```hs
ghci> :kind (|.|)
(|.|) :: (k -> *) -> (k1 -> k) -> k1 -> * -- (* -> *) -> (* -> *) -> * -> *

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)

-- f, g оба функторы, иначе не работает, ибо делаем композицию функторов
instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b -- сигнатура, функция -> функтор -> функтор
    fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b -- то же, в префиксной форме
    fmap h (Cmps x) = _ -- надо протащить h через f, g
-- имеем x :: f (g a)
-- h :: a -> b
-- f, g это функторы, протаскивание функции делается через fmap
-- допустим, мы сделаем функцию phi :: g a -> g b
-- тогда (fmap phi x) :: f (g b)
-- что такое phi? Это `(fmap h) :: g a -> g b` -- для любого функтора g
    fmap h (Cmps x) = Cmps fgb where
        fgb = fmap phi x where
            phi = fmap h
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

ghci> fmap succ $ Cmps (Just "abc")
Cmps {getCmps = Just "bcd"}
```
repl

```hs
https://stepik.org/lesson/30426/step/5?unit=11043
TODO

Сделайте тип `Cmps3`
представителем класса типов `Functor`
при условии, что первые его три параметра являются функторами:
{--
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

GHCi> fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}
--}
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance Functor (Cmps3 f g h) where
  fmap = undefined

-- solution

getCmps3 $ succ <$> Cmps3 (Just ["abc"])
Just ["bcd"]

```
test

### 1.5.6 первый закон функтора, доказательство

Докажем законы функтора для Compose
```hs
infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

{--
law #1, левый id
fmap id fa = fa

перепишем
fmap id fa = id fa
и в pointfree style
fmap id = id

если для f, g это верно, то и для композиции верно

fmap id (Cmps x)
=?
Cmps x

Cmps $ fmap (fmap id) x -- fmap id = id для функтора g
Cmps $ fmap id x -- fmap id = id для функтора f
Cmps $ id x -- id x = x
Cmps x
--}
```
repl

```hs
https://stepik.org/lesson/30426/step/7?unit=11043
TODO
{--
Докажите выполнение второго закона функторов для композиции двух функторов

fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)
aka
fmap f . fmap g = fmap (f . g)

- Write an answer
- Send your best submission to review
- Review submissions
- Wait for reviews of your solution
- Get points, max score is 3 points 
--}

-- solution

Такой формат задач хорош для традиционного класса, где все собрались и обсудили вопросы.
Для формата MOOC, где между студнем, классом и преподавателями пропасть, это не годится.
Кроме того, не для того я пошел в программисты, чтобы руками подстановки писать (это работа компилятора).

https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
> ... the second actually follows from the first and parametricity,
so you only need to sit down and prove one Functor law when you go to supply a Functor!

Докажем первый:

fmap id = id

дана реализация
fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
fmap h (Cmps x) = Cmps $ fmap (fmap h) x

предположим, что `fmap id = id` верно для f, g

докажем, что
fmap id (Cmps x)
=
Cmps x
где
x :: f (g a)

Cmps $ fmap (fmap id) x -- fmap id = id для функтора g, тогда:
Cmps $ fmap id x -- fmap id = id для функтора f, тогда:
Cmps $ id x -- id x = x, тогда:
Cmps x -- что и требовалось

-- Comment from teacher

Назовём наши функторы F⁡ и GG, то есть
Cmps x :: (F |.| G) a

Для удобства перепишем левую часть закона через композицию. Теперь требуется проверить, что:
(fmap h2 . fmap h1) (Cmps x) = fmap (h2 . h1) (Cmps x)

для произвольных
x  :: F (G a)
h1 :: a -> b
h2 :: b -> c

fmap h2 (fmap h1 (Cmps x)) ≡ fmap h2 (Cmps $ fmap (fmap h1) x)  -- def fmap
                           ≡ Cmps $ fmap (fmap h2) (fmap (fmap h1) x)  -- def fmap
                           ≡ Cmps $ (fmap (fmap h2) . fmap (fmap h1)) x  -- def (.)
                           = Cmps $ fmap (fmap h2 . fmap h1) x  -- Functor F
                           = Cmps $ fmap (fmap (h2 . h1)) x  -- Functor G

fmap (h2 . h1) (Cmps x) ≡ Cmps $ fmap (fmap (h2 . h1)) x  -- def fmap

```
test

### 1.5.8 Applicative для композиции аппликативов, pure

Композиция функторов является функтором. Доказано.
Что про аппликативные функторы? Композиция аппликативов будет аппликативным функтором?
Да.
```hs
infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure
    (<*>) = undefined

getCmps (pure 42 :: (|.|) [] [] Int)
[[42]]

ghci> getCmps (pure 42 :: ([] |.| [] |.| []) Int)
[Cmps {getCmps = [[42]]}]
```
repl

```hs
https://stepik.org/lesson/30426/step/9?unit=11043
TODO
{--
Напишите универсальные функции

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))

позволяющие избавляться от синтаксического шума для композиции нескольких функторов:

GHCi> pure 42 :: ([] |.| [] |.| []) Int
Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
GHCi> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
[[[42]]]
GHCi> unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
[Just [42]]
GHCi> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
[[[[42]]]]
--}
unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = undefined

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = undefined

-- solution
-- в совсем современном Haskell активно используют coerce из Data.Coerce

-- pure 42 :: ([] |.| [] |.| []) Int
-- getCmps (pure 42 :: ([] |.| [] |.| []) Int)

```
test

### 1.5.10 Applicative композиции, `applied over <*>`, семантика

Какая будет семантика апплая для композиции аппликативов?
Аппликатив, вычисление с эффектами, в некотором контексте.

Для аппликатива списка: список функций "апплай овер" список значений (с вложенным циклом).
Для аппликатива мейби: функция "апплай овер" либо ничего, либо значение.
Композиция списка и мейби не коммутативна, будут разные результаты: список опций или опция списка.

```hs
infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show) -- design time wrapper
instance (Functor f, Functor g) => Functor (f |.| g) where
    -- fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x
instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure
    (<*>) = undefined

-- examples

ghci> getCmps $ Cmps [Just (+1), Just (+2)] <*> Cmps [Just 30, Just 40]
[Just 31,Just 41,Just 32,Just 42]
-- оператор ап, слева композиция списка опций, справа композиция списка опций
-- [Just (+1), Just (+2)]
-- [Just 30, Just 40]
ghci> [(+1), (+2)] <*> [30, 40]
[31,41,32,42]

-- переставим местами конструкторы в композиции аппликативов
ghci> getCmps $ Cmps [Just (+1), Just (+2)] <*> Cmps [Nothing, Just 40]
[Nothing,Just 41,Nothing,Just 42]
ghci> getCmps $ Cmps (Just [(+1), (+2)]) <*> Cmps (Just [30, 40])
Just [31,41,32,42]
ghci> getCmps $ Cmps (Just [(+1), (+2)]) <*> Cmps (Nothing)
Nothing
```
repl

### 1.5.11 Applicative композиции, `applied over <*>`, реализация

Продолжение про реализацию `<*>` для композиции аппликативных функторов.
```hs
infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show) -- design time wrapper
instance (Functor f, Functor g) => Functor (f |.| g) where
    -- fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x
instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure

    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: (|.|) f g (a -> b) -> (|.|) f g a -> (|.|) f g b -- в развернутом виде
    Cmps h <*> Cmps x = Cmps $ (fmap (<*>) h) <*> x

-- разберем реализацию исходя из типов
-- (|.|) h -> (|.|) x -> (|.|) y -- по этой сигнатуре получаем заготовку реализации:
-- Cmps h <*> Cmps x = Cmps y -- внешние типы, контексты, где:
h :: f (g (a -> b))
x :: f (g a)
y :: f (g b)
-- fmap (<*>) h -- поднимаем оператор ап в функтор эф, там внутри этот оператор применится к функтору же,
-- в котором лежат функции a -> b:
<*> :: g (a -> b) -> (g a -> g b) -- по определению оператора ап
fmap <*> :: f (g (a -> b)) -> f (g a -> g b) -- по типу видно, что аргумент это h
fmap <*> h :: f (g a -> g b) -- стрелка, дающая на выходе то, что нам надо, если поднять это в контейнер f, который `x`

-- осталось проверить, выполняются ли 4 закона аппликативных фукторов для композиции, при условии,
-- что композиция из аппликативных функторов для которых законы выполняются.

-- alternative
Cmps f1 <*> Cmps f2 = Cmps (liftA2 (<*>) f1 f2)
```
repl


grep `TODO` markers, fix it.
