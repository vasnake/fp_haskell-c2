# FP Haskell, chapter 3, монады и эффекты

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-3/test-monads.hs)

Мотивация: ???

definitions: ???

## chapter 3.1, Монада Except

https://stepik.org/lesson/30722/step/1?unit=11809

- Except: монада для обработки исключений
- Реализация монады Except
- Реализация стандартного интерфейса для Except
- Пример использования монады Except
- Except как MonadPlus
- Except как MonadPlus: использование guard
- Except как MonadPlus: использование msum

### 3.1.2 newtype Except

Ранее мы рассмотрели набор монад (Reader, Writer, State, ...) и их интерфейс (ask, tell, ...).
Эти монады были сделаны как врапперы типов (стрелка, пара, ...).

Теперь рассмотрим монаду `Except` как враппер для `Either`, и ее интерфейс (throw, catch)
```hs
import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
import Control.Applicative (Alternative(..))

newtype Except e a = Except { runExcept :: Either e a } deriving Show
except :: Either e a -> Except e a
except = Except
-- except, runExcept это дуальные функции, для создания и разбора иксепшена

-- чтобы сделать иксепшн монадой, надо сделать функтор и аппликативный функтор (иерархия классов требует),
-- покажем стандартную заглушку для случаев, когда мы хотим реализовать именно монаду

instance Functor (Except e) where -- двух-параметрический конструктор, первый параметр надо связать для соответствия тайп-классу функтора
    fmap = liftM -- мы сделаем монаду, поэтому забежим вперед и сошлемся на нее
instance Applicative (Except e) where
    pure = return
    (<*>) = ap
```
repl

```hs
https://stepik.org/lesson/30722/step/3?unit=11809
TODO
{--
Реализуйте функцию 

withExcept :: (e -> e') -> Except e a -> Except e' a

позволящую, если произошла ошибка, применить к ней заданное преобразование.
--}
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = undefined

-- solution

```
test

### 3.1.4 Monad Except

Реализуем монаду для типа Except, эффект: возможность наличия ошибки
```hs
instance Monad (Except e) where
    return a = Except (Right a) -- упаковка значения в контекст
    m >>= k = case runExcept m of
        Left e -> Except (Left e) -- была ошибка, продвигаем ее далее
        Right a -> k a -- k: Kleisli -- ошибки нет, работаем дальше

```
repl

### 3.1.5 throwE, catchE (Except)

Для работы с монадой Except есть две полезные функции: throwE, catchE
```hs
throwE :: e -> Except e a
throwE = except . Left

-- версия bind для обработки ошибки (функция над ошибкой, в отличие от bind, где функция над значением)
catchE :: Except e a -> (e -> Except e2 a) -> Except e2 a
m `catchE` h = case runExcept m of
    Left e -> h e
    Right a -> except (Right a)

-- пара catch, throw должна соответствовать закону
catchE (throwE e) h = h e

-- как с этим принято работать?
do { action; ... } `catchE` handler

```
repl

### 3.1.6 пример использование монады Except

Посмотрим на примеры использования монады Except:
код "бросающий" иксепшн и код с обработкой "отловленного" иксепшн
```hs
-- определим тип ошибки: деление на ноль или другое, тип-сумма
data DivByError = ErrZero String | ErrOther deriving (Eq, Show)

-- Except DivByError Double -- это конструктор типа, где DivByError это левый параметр иксепшна, Double это правый параметр
(/?) :: Double -> Double -> Except DivByError Double -- double -> double -> either
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y -- return упаковывает результат в монаду иксепшна

-- пример отлавливания ошибки и ея обработки (в строку)
example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
    action = do
        q <- x /? y
        return $ show q
    handler = \err -> return $ show err
-- обработка ошибки упаковывает строку (с ошибкой) в монаду иксепшна, но через return.
-- это значит, что результат функции всегда будет ОК, т.е. в правой части Either
-- и мы тут как бы теряем информацию о наличии ошибки, но это норм, ибо мы уже обработали ея.

ghci> runExcept $ example0 5 2
Right "2.5" -- right
ghci> runExcept $ example0 5 0
Right "ErrZero \"5.0/0;\"" -- right, факт ошибки спрятан
-- для игрущечного примера и так сойдет.
```
repl

```hs
https://stepik.org/lesson/30722/step/7?unit=11809
TODO
{--
В модуле `Control.Monad.Trans.Except` библиотеки `transformers` 
имеется реализация монады `Except` 
с интерфейсом, идентичным представленному в видео-степах, но с более общими типами. 
Мы изучим эти типы в следующих модулях, однако 
использовать монаду `Except` из библиотеки `transformers` мы можем уже сейчас.

Введём тип данных для представления ошибки обращения к списку по недопустимому индексу:

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

Реализуйте оператор `!!!` доступа к элементам массива по индексу, 
отличающийся от стандартного `(!!)` поведением в исключительных ситуациях. 
В этих ситуациях он должен выбрасывать подходящее исключение типа `ListIndexError`

(!!!) :: [a] -> Int -> Except ListIndexError a 

GHCi> runExcept $ [1..100] !!! 5
Right 6
GHCi> (!!!!) xs n = runExcept $ xs !!! n
GHCi> [1,2,3] !!!! 0
Right 1
GHCi> [1,2,3] !!!! 42
Left (ErrIndexTooLarge 42)
GHCi> [1,2,3] !!!! (-3)
Left ErrNegativeIndex
--}
infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) = undefined

-- solution
-- на бесконечном списке работает (должно)

```
test

```hs
https://stepik.org/lesson/30722/step/8?unit=11809
TODO
{--
Реализуйте функцию `tryRead`, получающую на вход строку и пытающуюся всю эту строку превратить в значение заданного типа. 
Функция должна возвращать ошибку в одном из двух случаев: 
если вход был пуст или если прочитать значение не удалось.

Информация об ошибке хранится в специальном типе данных:

data ReadError = EmptyInput | NoParse String
  deriving Show

GHCi> runExcept (tryRead "5" :: Except ReadError Int)
Right 5
GHCi> runExcept (tryRead "5" :: Except ReadError Double)
Right 5.0
GHCi> runExcept (tryRead "5zzz" :: Except ReadError Int)
Left (NoParse "5zzz")
GHCi> runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ()))
Right (True,())
GHCi> runExcept (tryRead "" :: Except ReadError (Bool, ()))
Left EmptyInput
GHCi> runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
Left (NoParse "wrong")
--}
tryRead :: Read a => String -> Except ReadError a
tryRead = undefined

-- solution

```
test

> reads как бы принимает в качестве аргумента кроме строки еще и тип 
(а точнее реализацию представителя класса типов). 
И если компилятор способен вывести этот тип самостоятельно, он его подставит сам, а если нет - придется это сделать самостоятельно
```hs
-- class Read a where ...
data Read' a =
  Read'
    { read'  :: String -> a
    , reads' :: String -> [(a, String)]
    }

-- instance Read Int where ...
intReadInstance :: Read' Int
intReadInstance =
  Read'
    { read' = read   -- :: String -> Int
    , reads' = reads -- :: String -> [(Int, String)]
    }

-- instance Read Bool where ...
boolReadInstance :: Read' Bool
boolReadInstance =
  Read'
    { read' = read   -- :: String -> Bool
    , reads' = reads -- :: String -> [(Bool, String)]
    }

--readPair :: (Read a, Read b) => String -> String -> (a, b)
--readPair sa sb = (read sa, read sb)
readPair' :: Read' a -> Read' b -> String -> String -> (a, b)
readPair' instA instB sa sb = (read' instA sa, read' instB sb)

GHCi> reads' intReadInstance "1"
[(1,"")]
GHCi> reads' boolReadInstance "1"
[]
GHCi> :t read
read :: Read a => String -> a
GHCi> :t read'
read' :: Read' a -> String -> a
GHCi> readPair' intReadInstance intReadInstance "5" "6"
(5,6)
```
extra

```hs
https://stepik.org/lesson/30722/step/9?unit=11809
TODO
{--
Используя `tryRead` из прошлого задания, реализуйте функцию `trySum`, 
которая получает список чисел, записанных в виде строк, и суммирует их. 
В случае неудачи, функция должна возвращать информацию об ошибке 
вместе с номером элемента списка (нумерация с единицы), вызвавшим ошибку.

Для хранения информации об ошибке и номере проблемного элемента используем новый тип данных:

data SumError = SumError Int ReadError
  deriving Show

GHCi> runExcept $ trySum ["10", "20", "30"]
Right 60
GHCi> runExcept $ trySum ["10", "20", ""]
Left (SumError 3 EmptyInput)
GHCi> runExcept $ trySum ["10", "two", "30"]
Left (SumError 2 (NoParse "two"))

Подсказка: функция `withExcept` в этом задании может быть чрезвычайно полезна. 
Постарайтесь максимально эффективно применить знания, полученные на прошлой неделе.
https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Except.html#v:withExcept
--}
trySum :: [String] -> Except SumError Integer
trySum = undefined

-- solution
-- https://hoogle.haskell.org/?hoogle=withExcept&scope=set%3Ahaskell-platform
-- проверить решение не только на примерах, но и на пустом и бесконечном списках

```
test

### 3.1.10 MonadPlus Except

Иксепшн как монада мы рассмотрели.

Рассмотрим пользу от иксепшн как `MonadPlus` (к монаде добавлена моноидальная структура).
Оператор альтернативы позволит нам "пробовать" вычисление, если оно "упало", то пробовать следующее ...
Инфа об ошибка накапливается ...
```hs
-- реализация альтернатив через MonadPlus (для дальнейшего изложения не существенно)
-- ограничение моноида дает возможность накапливать инфу об ошибках
instance (Monoid e) => Alternative (Except e) where 
    empty = mzero
    (<|>) = mplus

-- вот самое интересное, моноидальное накапливание ошибок и поддержка альтернативных вычислений,
-- если предыдущие (в пайплайне) сломались
instance (Monoid e) => MonadPlus (Except e) where
    mzero = Except (Left mempty) -- throwE mempty -- создается ошибка на нейтральном содержимом

    -- склейка двух ошибок или проброс результата
    (Except x) `mplus` (Except y) = Except (
        case x of
            Left e -> either (Left . mappend e) Right y -- если левый шаг пайплайна содержит ошибку
            -- результат определяется содержимым правого шага.
            -- три аргумента у функции either, функция из `y :: Either e a`
            -- сделает либо ошибку, либо результат (в зависимости от содержимого y)
            -- если в у ошибка, то две ошибки суммируются в моноиде,
            -- если в у результат, то он прокидывается дальше (см. ветку `r -> r`)
            r -> r -- если левый шаг пайплайна без ошибки, он дальше и пойдет
    )
    -- x `mplus` y = withExcept mappend x `catchE` flip withExcept y

-- трансформер either, смотрит на содержимое третьего параметра (either) и
-- запускает либо первую функцию, либо вторую
either :: (a -> c) -> (b -> c) -> Either a b -> c

-- альтернативная реализация монады, с исполозованием функции either

instance Monad (Except e) where
    return = except . pure
    m >>= k = either throwE k (runExcept m)

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` handler = either handler pure (runExcept m)

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f = either (throwE . f) pure . runExcept
```
repl

Скандальная правда об обработке исключений в Haskell https://eax.me/haskell-exceptions/

### 3.1.11 демонстрация MonadPlus Except на кастомной ошибке

Ошибка должна быть моноидом, чтобы это (MonadPlus Except) работало.
```hs
instance Monoid DivByError where
    mempty = ErrOther
    ErrOther `mappend` ErrOther = ErrOther
    ErrOther `mappend` (ErrZero s2) = ErrZero s2
    (ErrZero s1) `mappend` ErrOther = ErrZero s1
    (ErrZero s1) `mappend` (ErrZero s2) = ErrZero $ s1 ++ s2

instance Semigroup DivByError where
    (<>) = mappend

-- было
example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
    action = do
        q <- x /? y
        return $ show q
    handler = return . show -- handler = \err -> return $ show err

-- стало
example2 :: Double -> Double -> Except DivByError String
example2 x y = action `catchE` handler where
    action = do
        q <- x /? y
        guard (y >= 0) -- результат использования альтернативы, вернет mempty на условии `y < 0` а mempty = ErrOther
        return (show q) -- нет ошибки
    handler (ErrZero s) = return s
    handler ErrOther = return ("negative y: " ++ show y)

-- демонстрация срабатывания mzero (mempty)
ghci> runExcept $ example2 5 0
Right "5.0/0;"
ghci> runExcept $ example2 5 2
Right "2.5"
ghci> runExcept $ example2 5 (-2)
Right "negative y: -2.0"

```
repl

### 3.1.12 демонстрация "поиска" рабочей операции в цепочке

Посмотрим на накопление ошибок в (монадической) цепочке вычислений, используем `msum`
для построения цепочки
```hs
ghci> runExcept $ msum [5 /? 0, 7 /? 0, 2 /? 0]
Left (ErrZero "5.0/0;7.0/0;2.0/0;") -- все ошибки

ghci> runExcept $ msum [5 /? 0, 7 /? 0, 2 /? 3]
Right 0.6666666666666666 -- встретилась рабочая операция

ghci> runExcept $ msum [5 /? 3, 7 /? 0, 2 /? 3]
Right 1.6666666666666667 -- прокидывается первая рабочая

```
repl

```hs
https://stepik.org/lesson/30722/step/13?unit=11809
TODO
{--
Тип данных для представления ошибки обращения к списку по недопустимому индексу `ListIndexError`
не очень естественно делать представителем класса типов `Monoid`. 
Однако, если мы хотим обеспечить накопление информации об ошибках, моноид необходим. 
К счастью, уже знакомая нам функция `withExcept`
позволяет изменять тип ошибки при вычислении в монаде `Except`

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

withExcept :: (e -> e') -> Except e a -> Except e' a
https://hoogle.haskell.org/?hoogle=withExcept&scope=set%3Ahaskell-platform

Сделайте тип данных `SimpleError`
представителем необходимых классов типов и 
реализуйте преобразователь для типа данных ошибки `lie2se` так, чтобы обеспечить следующее поведение

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)

lie2se :: ListIndexError -> SimpleError 

GHCi> toSimple = runExcept . withExcept lie2se
GHCi> xs = [1,2,3]
GHCi> toSimple $ xs !!! 42
Left (Simple {getSimple = "[index (42) is too large]"})
GHCi> toSimple $ xs !!! (-2)
Left (Simple {getSimple = "[negative index]"})
GHCi> toSimple $ xs !!! 2
Right 3
GHCi> import Data.Foldable (msum)
GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
Left (Simple {getSimple = "[negative index][index (42) is too large]"})
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
Right 3
--}
lie2se :: ListIndexError -> SimpleError
lie2se = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/30722/step/14?unit=11809
TODO
{--
Стандартная семантика `Except` как аппликативного функтора и монады: выполнять цепочку вычислений до первой ошибки. 
Реализация представителей классов `Alternative` и `MonadPlus` наделяет эту монаду альтернативной семантикой: 
попробовать несколько вычислений, вернуть результат первого успешного, а в случае неудачи — все возникшие ошибки.

Довольно часто возникает необходимость сделать нечто среднее. 
К примеру, при проверке корректности заполнения анкеты или при компиляции программы для общего успеха необходимо, 
чтобы ошибок совсем не было, но в то же время, нам хотелось бы не останавливаться после первой же ошибки, 
а продолжить проверку, чтобы отобразить сразу все проблемы. 
`Except` такой семантикой не обладает, но никто не может помешать нам сделать свой тип данных 
(назовем его `Validate`), 
представители которого будут обеспечивать требую семантику, позволяющую сохранить список всех произошедших ошибок:

newtype Validate e a = Validate { getValidate :: Either [e] a }

Реализуйте функцию 
validateSum :: [String] -> Validate SumError Integer

GHCi> getValidate $ validateSum ["10", "20", "30"]
Right 60
GHCi> getValidate $ validateSum ["10", "", "30", "oops"]
Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]

Эта функция практически ничем не отличается от уже реализованной ранее `trySum`, если использовать функцию-адаптер 
collectE :: Except e a -> Validate e a 
и представителей каких-нибудь классов типов для `Validate`
--}
collectE :: Except e a -> Validate e a
collectE = undefined

validateSum :: [String] -> Validate SumError Integer
validateSum = undefined

-- solution

```
test

## chapter 3.2, Монада Cont

https://stepik.org/lesson/30723/step/1?unit=11811

- Continuation passing style
- Программирование в CPS-стиле
- Монада Cont
- Монада Cont: реализация
- Монада Cont: эффекты
- Стандартный интерфейс Cont: функция callCC
- Реализация callCC

### 3.2.2 Continuation Passing Style, CPS

Нас интересует монада Continuation, для управления продолжением вычислений.
Чтобы понять, как это, надо разобраться с Continuation Passing Style,
стиль передачи продолжений.
По сути, это композиция функций, когда внутренняя функция явно принимает враппер в виде аргумента.
Можно смотреть на это как на передачу callback функции в вычисление.

Если прищурится, можно разглядеть, как CPS позволяет создавать DSL (Domain Specific Language)
```hs
-- сетап
{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
import Control.Monad ( when )

-- возьмем две обычные функции (square, add)
square :: Int -> Int
square x = x ^ 2

add :: Int -> Int -> Int
add x y = x + y

-- превратим их в CPS

-- функция принимает некий дополнительный аргумент
-- WIP:

square :: Int -> (Int -> r) -> ?
square x c = x ^ 2

add :: Int -> Int -> (Int -> r) -> ?
add x y c = x + y

-- этот аргумент применяется к результату

square :: Int -> (Int -> r) -> r
square x c = c (x ^ 2)

add :: Int -> Int -> (Int -> r) -> r
add x y c = c (x + y)

-- получаем механизм типа коллбеков, или, с другой стороны, композицию функций

-- examples

ghci> square 2 id
4
ghci> square 2 show
"4"
ghci> add 2 3 print
5

-- а что если передать в качестве конт. еще один конт?
ghci> :t square 2 square
square 2 square :: (Int -> r) -> r -- нужно скормить в выражение еще одну (терминирующую) функцию, чтобы получить результат

ghci> square 2 square id
16

ghci> square 2 (add 3) (add 5) id
12 -- 2^2 + 3 + 5

-- получаем композицию функций но в стиле CPS

```
repl

```hs
https://stepik.org/lesson/30723/step/3?unit=11811
TODO
{--
CPS-преобразование часто применяют для создания предметно-ориентированных языков (DSL).
https://ru.wikipedia.org/wiki/Предметно-ориентированный_язык

Реализуйте комбинаторы, которые позволят записывать числа вот в таком забавном формате:

GHCi> decode one hundred twenty three as a number
123
GHCi> decode one hundred twenty one as a number
121
GHCi> decode one hundred twenty as a number
120
GHCi> decode one hundred as a number
100
GHCi> decode three hundred as a number
300
GHCi> decode two thousand seventeen as a number
2017

Достаточно чтобы работали всякие простые случаи, как в примерах; 
не старайтесь поддержать прямо все допустимые варианты, это потребует несколько бóльших усилий.
--}
decode = undefined
as = undefined
a = undefined
number = undefined

one = undefined
two = undefined
three = undefined
seventeen = undefined
twenty = undefined
hundred = undefined
thousand = undefined

-- solution

```
test

### 3.2.4 нелинейный пайплайн в CPS

Как насчет менее прямолинейных пайплайнов?
```hs
-- имея две такие функции
square :: Int -> (Int -> r) -> r
square x c = c (x ^ 2)
add :: Int -> Int -> (Int -> r) -> r
add x y c = c (x + y)

-- напишем функцию суммы квадратов в CPS

sumSquares :: Int -> Int -> (Int -> r) -> r
sumSquares x y c = square x cont1 where
    cont1 x2 = square y cont2 where
        cont2 y2 = add x2 y2 cont3 where
            cont3 ss = c ss

-- отрефакторим немного
sumSquares x y c = square x (\x2 -> square y (\y2 -> add x2 y2 (\ss -> c ss)))
-- и еще немного
sumSquares x y c = 
    square x $ \x2 -> 
    square y $ \y2 -> 
    add x2 y2 $ \ss -> -- тут мы видим некотурую нелинейность
    c ss
-- напоминает do-нотацию монады, не так ли?

ghci> sumSquares 3 4 show
"25"
```
repl

### 3.2.5 newtype Cont

Как могло бы выглядеть применение монады Cont, примеры
```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
evalCont :: Cont r r -> r -- удобный враппер терминирования конт. на функции id (гарантирует совпадение типа а с типом эр)
evalCont m = runCont m id

-- забегая вперед (монаду мы еще не определили),
-- как будут выглядеть вычисления в этой монаде Cont

square :: Int -> Cont r Int
square x = return $ x ^ 2

add :: Int -> Int -> Cont r Int
add x y = return $ x + y

-- сумма квадратов CPS в ду-нотации
sumSquares :: Int -> Int -> Cont r Int
sumSquares x y = do
    x2 <- square x
    y2 <- square y
    ss <- add x2 y2
    return ss
-- сравните с безмонадным CPS
sumSquares x y c = 
    square x $ \x2 -> 
    square y $ \y2 -> 
    add x2 y2 $ \ss -> -- тут мы видим некотурую нелинейность
    c ss

runCont (square 2) show -- "4"
evalCont (square 2) -- 4
evalCont (square 2 >>= (add 3) >>= (add 5)) -- 12
evalCont $ sumSquares 3 4 -- 25
```
repl

```hs
https://stepik.org/lesson/30723/step/6?unit=11811
TODO
{--
Реализуйте функцию `showCont`, запускающую вычисление и возвращающую его результат в виде строки.
--}
showCont :: Show a => Cont String a -> String
showCont = undefined

-- solution

```
test

### 3.2.7 Monad Cont

Реализуем монаду Cont (return, bind).
Композиция "продолжений", реализованных через лямбды.

Интересно, что тут надо считать "эффектом"?
Эффектами монады Cont будут: возможность управления вычислениями (прерывание, восстановление, ...)
```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
evalCont :: Cont r r -> r -- удобный враппер терминирования конт. на функции id (гарантирует совпадение типа а с типом эр)
evalCont m = runCont m id

instance Monad (Cont r) where
    return :: a -> Cont r a
    return x = Cont (\c -> c x) -- монада над стрелочным типом, поэтому лямбда

    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b -- m bind k: монада бинд стрелкаКлейсли, на выходе монада
    (Cont v) >>= k = Cont (\c -> v (\a -> runCont (k a) c)) -- лямбды, ибо работаем со стрелочным типом
    -- нам должны дать конт. си, тогда мы запустим вычисление `v (...)`
    -- в котором нам должны дать аргумент для стрелкаКлейсли

-- распишем как выглядит байнд после распаковки враппера Cont
-- ((a -> r) -> r) -- левый параметр, монада
-- (a -> (b -> r) -> r) -- правый параметр, стрелка Клейсли
-- (b -> r) -> r -- возвращаемое значение, монада
bind :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
bind v k = \c -> -- лямбда ибо возвращаемое значение `(b -> r) -> r`
    v (\a -> k a c) -- вторая лямбда, ибо связываем два вычисления
    -- переданный снаружи конт. идет вторым параметром стрелки клейсли
-- по факту, байнд это функция трех аргументов, третий: тот самый конт. который мы ждем:
-- (b -> r) -- id или show в наших примерах
bind v k c = v (\a -> k a c)
-- стрелка Клейсли `(a -> (b -> r) -> r)`
-- это по типу `a -> r` и это именно то, чего ждет в первом параметре наша монада в левой части, `v`
-- таким образом, континуэйшены образуют некую матрешку, выполнение которой идет сверху вниз (слева-направо),
-- когда результат левого вычисления передается в стрелку, данную в виде параметра
-- т.е. из-за лямбд, вычисления справа (по ходу записи выражений), откладываются на "потом", после выполнения выражений слева.

```
repl

> Computations which can be interrupted and resumed.
... manipulation of the continuation functions can achieve complex manipulations of the future of the computation, 
such as interrupting a computation in the middle, 
aborting a portion of a computation, 
restarting a computation, and interleaving execution of computations ...
Many algorithms which require continuations in other languages do not require them in Haskell, due to Haskell's lazy semantics

### 3.2.8 эффекты монады Cont

Посмотрим, как пользваться эффектами монады Cont: прерывание, восстановление, ...
```hs
sumIt :: Cont r Integer
sumIt = do
    a <- return 3
    b <- return 5
    return (a + b)

ghci> runCont sumIt id
8

-- заменим return на его определение
sumIt = do
    a <- return 3
    b <- Cont (\c -> c 5) -- появился доступ к конт. можно что-то нахимичить
    return (a + b)

-- при таком вмешательстве в пайплайн, пропала полиморфность по `r`
sumIt :: Cont String Integer
sumIt = do
    a <- return 3
    b <- Cont (\c -> "STOP") -- не используем применение конт. (c 5) здесь, вычисления останавливаются
    return (a + b)

ghci> runCont sumIt show -- так сработало
"STOP"
ghci> runCont sumIt id -- а так сломалось на типах
<interactive>:26:15: error: • Couldn't match type ‘Integer’ with ‘[Char]’
-- id требует, чтобы вход и выход совпадали по типам. show этого не требует

-- другой вариант вмешательства в пайплайн:
sumIt :: Cont Integer Integer
sumIt = do
    a <- return 3
    b <- Cont (\_ -> 42) -- по прежнему нет применения конт.; тип значения поменялся
    return (a + b)

ghci> runCont sumIt show -- и шоу перестал работать, ибо show :: Integer -> String в данном случае
<interactive>:35:15: error: • Couldn't match type ‘[Char]’ with ‘Integer’
ghci> runCont sumIt id -- id :: Integer -> Integer работает
42

-- это были примеры управления конт., игнорирование "продолжения".

-- можно вмешаться иначе, вызовем конт. два раза
sumIt :: Cont [r] Integer
sumIt = do
    a <- return 3
    b <- Cont (\c -> (c 4) ++ (c 5)) -- допустим, мы хотим список из двух результатов, тогда сигнатура это отразит: `[r]`
    -- если бы здесь было бы еще что-то, то список (из двух элементов) дал бы разветвление вычислений (как монада списка)
    return (a + b)

ghci> runCont sumIt show -- шоу на выходе дает список, поэтому OK
"78" -- это список из 7 и 8
```
repl

Видно, что заменяя шаги пайплайна (монадических вычислений) на кастомные, мы можем управлять ходом вычислений.

```hs
https://stepik.org/lesson/30723/step/9?unit=11811
TODO
{--
Возможность явно работать с продолжением обеспечивает доступ к очень гибкому управлению исполнением. 
В этом задании вам предстоит реализовать вычисление, которое 
анализирует и модифицирует значение, возвращаемое кодом, написанным после него.

В качестве примера рассмотрим следующую функцию:

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

Эта функция принимает значение `x1`, 
совершает с ним последовательность операций (несколько раз прибавляет 10) и 
после каждой операции «сохраняет» промежуточный результат. 
При запуске такой функции используется дополнительный предикат, который является критерием «корректности» результата, 
и в случае, если возвращенное функцией значение этому критерию не удовлетворяет, 
вернется последнее удовлетворяющее ему значение из «сохраненных»:

GHCi> runCheckpointed (< 100) $ addTens 1
31
GHCi> runCheckpointed  (< 30) $ addTens 1
21
GHCi> runCheckpointed  (< 20) $ addTens 1
11
GHCi> runCheckpointed  (< 10) $ addTens 1
1

(Если ни возвращенное, ни сохраненные значения не подходят, 
результатом должно быть первое из сохраненных значений; 
если не было сохранено ни одного значения, то результатом должно быть возвращенное значение.)

Обратите внимание на то, что функция `checkpoint` передается в `Checkpointed` вычисление как параметр, 
поскольку её поведение зависит от предиката, который будет известен только непосредственно при запуске.
--}
type Checkpointed a = ??

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed = undefined

-- solution
{--
> В задании сбивает с толку фраза, что необходимо как бы заглянуть в прошлое, 
вернув, если предикат не срабатывает, "сохраненное" значение. 
На самом деле продолжения - это о будущих вычислениях, поэтому нужно наоборот - 
заглянуть в будущее, передав предикату результат следующего вычисления, и, 
если он не удовлетворяет предикату, то вернуть то значение, которое есть в настоящем
--}

```
test

```hs
https://stepik.org/lesson/30723/step/10?unit=11811
TODO
{--
Вычисление в монаде `Cont` передает результат своей работы в функцию-продолжение. 
А что, если наши вычисления могут завершиться с ошибкой? 
В этом случае мы могли бы явно возвращать значение типа `Either` и каждый раз обрабатывать два возможных исхода, что не слишком удобно. 
Более разумный способ решения этой проблемы предоставляют трансформеры монад, но с ними мы познакомимся немного позже.

Определите тип данных `FailCont` для вычислений, которые получают два продолжения и вызывают 
одно из них в случае успеха, а другое — при неудаче. 
Сделайте его представителем класса типов `Monad` и 
реализуйте вспомогательные функции `toFailCont` и `evalFailCont`, 
используемые в следующем коде:

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

(Здесь используется функция `tryRead` из предыдущего урока; определять её заново не надо.)

GHCi> evalFailCont $ addInts "15" "12"
Right 27
GHCi> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
Oops: EmptyInput
--}
newtype FailCont r e a = FailCont { runFailCont :: ?? }

toFailCont :: Except e a -> FailCont r e a
toFailCont = undefined

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont = undefined

-- solution

```
test

### 3.2.11 использование callCC для прерывания

Вспомогательные функции для `Cont`: `callCC`
позволяет прервать цепочку вычислений (по условию) без потери полиморфности по возвращаемому значению
```hs
-- callCC - call with current continuation

-- пример, один из эффектов: ранний выход, прерывание цепочки (через игнорирование конт., не-применение его внутри лямбды)
test :: Integer -> Cont Integer Integer
test x = do
    a <- return 3
    -- вместо `return ()` вмешаемся в вызов продолжения:
    Cont (\c -> if x > 100 then 42 else c ()) -- выбор ветки исполнения следующего шага, в зависимости от аргумента
    -- если х больше 100 то выходим, не вывываем продолжение, иначе вызываем `c ()`, где юнит это просто заглушка   
    -- прерывание вычислений по условию
    return (a + x)
-- 42 портит нам полиморфизм, фиксируя параметр r в `Cont r a`

ghci> runCont (test 99) id
102
ghci> runCont (test 101) id
42

-- callCC решает эту проблему

-- для данного примера, сигнатура коллСС будет:
callCC :: ((Integer -> Cont r b) -> Cont r Integer) -> Cont r Integer
-- видно, что на входе функция, представляющая собой трансфорацию оригинальной функции
-- `(Integer -> Cont r b)` в желаемый результат `Cont r Integer`
-- эта функция строится по месту в виде лямбды, оборачивающей оригинальный код:
test2 :: Integer -> Cont r Integer
test2 x = callCC (\k -> do
    a <- return 3
    when (x > 100) (k 42) -- при выполнении условия здесь цепочка прерывается
    -- иначе игнор строки и переход на следующую
    return (a + x))
-- k :: Integer -> Cont r b -- это функция производящая продолжение
-- т.е. имеем две ветки: дергаем k или не дергаем k

-- полиморфизм не потерян, работает и с id и с show
ghci> runCont (test2 101) id
42
ghci> runCont (test2 99) id
102

ghci> runCont (test2 99) show
"102"
ghci> runCont (test2 101) show
"42"

```
repl

### 3.2.12 реализация callCC

Разберем, как устроена `callCC`
```hs
-- на примере применения callCC, разберем, что тут происходит
test2 :: Integer -> Cont r Integer
test2 x = callCC (\k -> do
    a <- return 3
    when (x > 100) (k 42) -- при выполнении условия здесь цепочка прерывается
    -- иначе игнор строки и переход на следующую, такова семантика
    return (a + x))
-- k :: Integer -> Cont r b -- это функция производящая продолжение
-- В итоге имеем две ветки вычислений: дергаем k или не дергаем k

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont (\c -> runCont (f (\a -> Cont (\_ -> c a))) c)
-- `c` это передаваемый снаружи "терминатор" (или следующее продолжение)

-- вся соль здесь:
f (\a -> Cont (\_ -> c a)) -- параметр `\a -> Cont (\_ -> c a)` это функция, которая
-- либо вызывается (когда надо прервать цепочку), либо нет. Это `k` в примере.
-- Если нет, то на этом месте (вызов `f`) будет полное выражение цепочки Cont как-бы-без-эффекта.
-- Если да, то будет выражение прерывания `c 42` завернутое в Cont.

-- функция `f` это лямбда, которую мы построили
\k -> do
    a <- return 3
    when (x > 100) (k 42)
    return (a + x)

-- разбор через подстановки подстановки
-- Cont r b = (b -> r) -> r
-- Cont r a = (a -> r) -> r
-- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC' :: ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r
callCC' f = \c -> f (\a -> c a) c
-- мы видели два сценария использования
callCC (\k -> expr) -- без вызова к
callCC (\k -> do {...; k 42; ...}) -- с вызовом к
k :: (a -> Cont r b)
-- т.е. при вызове callCC в первом случае мы не дергаем первый параметр `f (\a -> c a) c`
-- что значит: мы не вызываем продолжение, `(\a -> c a)` игнорится, в итоге callCC вырождается в
callCC' f = \c -> f expr c -- что эквивалентно цепочке конт. без эффекта прерывания.
-- Во втором случае, вместо к будет подставлена конструкция
-- `(\a -> c a)` и, в данном случае, параметр а заменяется аргументом 42
-- и вызывается продожение на этом аргументе
callCC' f = \c -> f (c 42) c

```
repl

```hs
https://stepik.org/lesson/30723/step/13?unit=11811
TODO
{--
Реализуйте функцию `callCFC` для монады `FailCont` по аналогии с `callCC`
--}
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC = undefined

-- solution

```
test

## chapter 3.3, Трансформеры монад

https://stepik.org/lesson/31556/step/1?unit=11810

- Трансформеры монад: постановка задачи
- Трансформеры монад: пример
- Трансформированная монада – это монада
- Стандартные монады – это трансформеры

### 3.3.2 два вычисления с разными эффектами

Аппликативные функторы и монады позволяют выполнять вычисления с эффектами.
Что делать, если эффектов нам надо несколько?
К примеру, записывать в лог, обрабатывать исключения, хранить состояние, читать окружение, делать IO, ...

В случае с Ап.функторами хорошо работает композиция: композиция двух аппликативов это аппликатив, см. Compose.

С монадами сложнее, композиция монад (в общем случае) не будет монадой.
Слишком сложная структура и поведение (зависимость структуры от вычислений).

Есть обходные пути, один из них: трансформер монад.

Подготовим песочницу для разбора трансформеров.
Подготовим два вычисления с разными эффектами.
Вернее, вычисление одно (вернуть второй элемент списка), но эффекты разные.
```hs
-- transformers, setup
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)

-- будем решать задачу объединения ридера и врайтера

-- эффект ридера, читает второй элемент из списка (строк)
secondElem :: Reader [String] String
secondElem = do
    el2 <- asks ((map toUpper) . head . tail)
    return el2

strings = ["ab", "cd", "fg"]

ghci> runReader secondElem strings -- читаем из "окружения"
"CD"

-- эффект врайтер, возвращает воторой элемент из списка, в лог пишет первый элемент
logFirst :: [String] -> Writer String String
logFirst xs = do
    let el1 = head xs
    let el2 = ((map toUpper) . head . tail) xs
    tell el1
    return el2

ghci> runWriter (logFirst strings) -- читаем из параметра
("CD","ab")

-- теперь мы хотим два эффекта соединить:
-- писать в лог первый элемент, возвращать второй,
-- при этом читать данные не из параметра, а из "окружения", как ридер
```
repl

### 3.3.3 композиция монад через трансформер

Одна монада (врайтер) объявляется внутренней.
Вторая (ридер) объявляется трансформером (суффикс T).
```hs
logFirstAndRetSecond :: 
    ReaderT [String]    -- трансформер, внешняя монада
    (Writer String)     -- внутренняя монада
    String              -- возвращаемый композицией тип
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift (tell el1) -- подъем из внутренней монады
    -- можно читать как "поднять API внутренней монады"
    return el2

-- Как этим пользоваться?

-- одно-параметрический тип, годится для подстановки в трансформер
ghci> :k Writer String
Writer String :: * -> *

-- эволюция кайндов
ghci> :k Reader
Reader :: * -> * -> * -- двух-параметрический
ghci> :k Reader [String]
Reader [String] :: * -> * -- одно-параметрический
ghci> :k ReaderT [String]
ReaderT [String] :: (* -> *) -> * -> * -- двух-параметрический, конструктор монады
-- принимает монаду `(* -> *)`
-- и возвращает монаду `* -> *`
ghci> :k ReaderT [String] (Writer String)
ReaderT [String] (Writer String) :: * -> * -- одно-параметрический, монада

-- как это запускать?
ghci> :t runReaderT
runReaderT :: ReaderT r m a -> r -> m a
-- ReaderT r m a -- три параметра для трансформера: окружение, внутренняя монада, возвращаемое значение (внутреннюю монаду)

ghci> :t runReaderT logFirstAndRetSecond -- первый параметр передан, хочет второй и вернет врайтер (внутреннюю монаду)
runReaderT logFirstAndRetSecond :: [String] -> Writer String String

ghci> :t runWriter (runReaderT logFirstAndRetSecond strings)
  :: (String, String)

ghci> runWriter (runReaderT logFirstAndRetSecond strings) -- окружение передано снаружи
("CD","ab")
```
repl

```hs
https://stepik.org/lesson/31556/step/4?unit=11810
TODO
{--
Перепишите функцию `logFirstAndRetSecond` из предыдущего видео, 
используя трансформер `WriterT` из модуля `Control.Monad.Trans.Writer` библиотеки `transformers`, и 
монаду `Reader` в качестве базовой.

GHCi> runReader (runWriterT logFirstAndRetSecond) strings
("DEFG","abc")
--}
logFirstAndRetSecond :: ??
logFirstAndRetSecond = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/31556/step/5?unit=11810
TODO
{--
Реализуйте функцию 
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]

Эта функция принимает два предиката и список и 
записывает в один лог элементы списка, удовлетворяющие первому предикату, 
в другой лог — второму предикату, 
а возвращающает список элементов, ни одному из них не удовлетворяющих.

GHCi> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
(([3,4,5,6,7],[0,1,2]),[8,9,10])
--}
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate = undefined

-- solution

```
test

### 3.3.6 удобная обвязка композиции в трансформере

Из трансформера вышел сложный интерфейс, мы хотим его замаскировать.
Сделаем вид, что у нас есть монада `MyRW`
```hs
ghci> runWriter (runReaderT logFirstAndRetSecond strings)
("CD","ab")

logFirstAndRetSecond :: 
    ReaderT [String]    -- трансформер, внешняя монада
    (Writer String)     -- внутренняя монада
    String              -- возвращаемый композицией тип
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift (tell el1) -- подъем из внутренней монады -- можно читать как "поднять API внутренней монады"
    return el2

-- запакуем его в монаду-оболочку, для удобства
type MyRW = ReaderT [String] (Writer String)

ghci> :k MyRW 
MyRW :: * -> * -- монада, одно-параметрический конструктор типов
-- предоставляет ask, tell методы
-- с учетом того, что tell надо лифтить

logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift (tell el1) -- подъем из внутренней монады -- можно читать как "поднять API внутренней монады"
    return el2

-- утилита запуска
runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)

ghci> runMyRW logFirstAndRetSecond strings
("CD","ab") -- пара: значение, лог

```
repl

```hs
https://stepik.org/lesson/31556/step/7?unit=11810
TODO
{--
Наша абстракция пока что недостаточно хороша, поскольку пользователь всё ещё должен помнить такие детали, как, например, то, что 
`asks` нужно вызывать напрямую, а `tell` — только с помощью `lift`.

Нам хотелось бы скрыть такие детали реализации, обеспечив унифицированный интерфейс доступа к 
возможностям нашей монады, связанным с чтением окружения, и к возможностям, связанным с записью в лог. 
Для этого реализуйте функции `myAsks` и `myTell`, позволяющие записать `logFirstAndRetSecond` следующим образом:

logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2
--}
myAsks :: ([String] -> a) -> MyRW a
myAsks = undefined

myTell :: String -> MyRW ()
myTell = undefined

-- solution

```
test

### 3.3.8 Reader, Writer, State определены как трансформеры

Монады Reader, Writer, State определены как трансформеры.
Т.е. способ получения монады через трансформер хорош настолько, 
что стандартные монады реализованы именно так.

Через внутреннюю монаду `Identity`
```hs
ghci> :i Reader
type Reader :: * -> * -> *
type Reader r = ReaderT r Identity :: * -> * -- Defined in ‘Control.Monad.Trans.Reader’

ghci> :t Reader
<interactive>:1:1: error: -- у нас нет конструктора `Reader`, это синоним, `type Reader`

-- доступен такой конструктор (трансформера)
ghci> :i reader
reader :: Monad m => (r -> a) -> ReaderT r m a -- Defined in ‘Control.Monad.Trans.Reader’
-- обычный ридер получится, если скормить ему монаду Identity

-- производительность страдать не должна, ибо айдентити сделан через newtype (убираемый в рантайм)

```
repl

```hs
https://stepik.org/lesson/31556/step/9?unit=11810
TODO
{--
Превратите монаду `MyRW` в трансформер монад `MyRWT`:

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
First is "abc"
Second is "DEFG"
("DEFG","abc")
--}
type MyRWT m = ??

runMyRWT :: ??
runMyRWT = undefined

myAsks :: Monad m => ??
myAsks = undefined

myTell :: Monad m => ??
myTell = undefined

myLift :: Monad m => ??
myLift = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/31556/step/10?unit=11810
TODO
{--
С помощью трансформера монад `MyRWT` мы можем написать безопасную версию `logFirstAndRetSecond`:

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
Just ("DEFG","abc")
GHCi> runMyRWT logFirstAndRetSecond ["abc"]
Nothing

Реализуйте безопасную функцию `veryComplexComputation`, 
записывающую в лог через запятую первую строку четной длины и первую строку нечетной длины, 
а возвращающую пару из второй строки четной и второй строки нечетной длины, приведенных к верхнему регистру:

GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
Nothing
GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
Just (("KL","HIJ"),"defg,abc")

Подсказка: возможно, полезно будет реализовать функцию `myWithReader`
https://hoogle.haskell.org/?hoogle=withReader&scope=set%3Ahaskell-platform
--}
veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/31556/step/11?unit=11810
TODO
{--
Предположим мы хотим исследовать свойства рекуррентных последовательностей. 
Рекуррентные отношения будем задавать вычислениями типа 
`State Integer Integer`, 
которые, будучи инициализированы текущим значением элемента последовательности, 
возвращают следующее значение в качестве состояния и текущее в качестве возвращаемого значения, например:

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

Используя монаду `State` из модуля `Control.Monad.Trans.State` и 
трансформер `ExceptT` из модуля `Control.Monad.Trans.Except` 
библиотеки `transformers`, реализуйте для монады

type EsSi = ExceptT String (State Integer)

функцию 
`runEsSi :: EsSi a -> Integer -> (Either String a, Integer)`
запускающую вычисление в этой монаде, 
а также функцию 
`go :: Integer -> Integer -> State Integer Integer -> EsSi ()`
принимающую шаг рекуррентного вычисления и два целых параметра, задающие нижнюю и верхнюю границы допустимых значений вычислений. 
Если значение больше или равно верхнему или меньше или равно нижнему, то оно прерывается исключением с соответствующим сообщением об ошибке

GHCi> runEsSi (go 1 85 tickCollatz) 27
(Right (),82)
GHCi> runEsSi (go 1 80 tickCollatz) 27
(Left "Upper bound",82)
GHCi> runEsSi (forever $ go 1 1000 tickCollatz) 27
(Left "Upper bound",1186)
GHCi> runEsSi (forever $ go 1 10000 tickCollatz) 27
(Left "Lower bound",1)
--}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = undefined 

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/31556/step/12?unit=11810
TODO
{--
Модифицируйте монаду `EsSi` из предыдущей задачи, 
обернув ее в трансформер `ReaderT` с окружением, 
представляющим собой пару целых чисел, задающих нижнюю и верхнюю границы для вычислений. 
Функции `go` теперь не надо будет передавать эти параметры, они будут браться из окружения. 
Сделайте получившуюся составную монаду трансформером:

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

Реализуйте также функцию для запуска этого трансформера

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                -> (Integer,Integer)  
                -> Integer 
                -> m (Either String a, Integer)

и модифицируйте код функции `go`, изменив её тип на

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()

так, чтобы для шага вычисления последовательности с отладочным выводом текущего элемента последовательности на экран

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

мы получили бы

GHCi> runRiiEsSiT (forever $ go tickCollatz') (1,200) 27
82
41
124
62
31
94
47
142
71
214
(Left "Upper bound",214)
--}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                 -> (Integer,Integer) 
                 -> Integer 
                 -> m (Either String a, Integer)
runRiiEsSiT = undefined 

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go = undefined

-- solution

```
test

## chapter 3.4, Трансформер ReaderT

https://stepik.org/lesson/38577/step/1?unit=17396

- Тип ReaderT
- Функтор Reader
- Функтор ReaderT
- Аппликативный функтор Reader
- Аппликативный функтор ReaderT
- Альтернативная реализация Applicative
- Монада ReaderT
- Класс MonadTrans и лифтинг
- Стандартный интерфейс для ReaderT

Ранее рассмотрели внешнюю сторону трансформеров (монад), их применение.
Теперь рассмотрим реализацию трансформеров.

Спойлер: ничего сложного, композиция: внутренняя монада заворачивается во внешнюю,
внешняя (зная все про себя и внешний интерфейс внутренней) реализует монадический интерфейс для композиции в целом.

### 3.4.2 ReaderT, конструктор reader

Как трансформеры создавать, самостоятельно.

Монаду `Reader` перепишем как трансформер (обвязку вокруг внутренней абстрактной монады) `ReaderT`.
Рядом с реализацией `Reader` будем писать `ReaderT` и смотреть на разницу.
```hs
{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов

newtype Reader r a = Reader { runReader :: r -> a } -- было
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } -- стало, m это еще один параметр, внутренняя монада

-- Ридер это функциональная стрелка, со связанным первым параметром.
-- Абстракция "чтения из окружения".

reader :: (Monad m) => (r -> a) -> ReaderT r m a -- конструктор
-- Нам нужен кастомный конструктор, чтобы иметь возможность упаковать стрелку `r -> a` в композитную монаду.
-- Если этого не сделать, конструктор `ReaderT` будет требовать стрелку в монаду `r -> m a`,
-- это не то, что мы хотим

runReader (Reader (*3)) 7 -- стрелка пакуется в монаду ридер, ок
runReaderT (ReaderT (func)) 7 where func = \x -> [x*3] -- для трансформера было бы так, внутренняя монада: список

reader :: (Monad m) => (r -> a) -> ReaderT r m a -- реализация
reader f = ReaderT (return . f) -- return after f, ретурн это стандартный способ заворачивания в монаду

:t runReaderT (reader (*3)) 7 -- выражение полиморфно по внутренней монаде, надо указать компайлеру, какая монада нам нужна
    :: (Monad m, Num a) => m a

ghci> runReaderT (reader (*3)) 7 :: [Int] -- внутренняя монада: список
[21]

runReaderT (reader (*3)) 7 :: Maybe Int
runReaderT (reader (*3)) 7 :: IO Int
```
repl

```hs
https://stepik.org/lesson/38577/step/3?unit=17396
TODO
{--
В задачах из предыдущих модулей мы сталкивались с типами данных
задающих вычисления с двумя и тремя окружениями соответственно.

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

Можно расширить их до трансформеров:

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

Напишите «конструирующие» функции

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a

обеспечивающие следующее поведение

GHCi> (getArr2T $ arr2 (+)) 33 9 :: [Integer]
[42]
GHCi> (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
Right 120
GHCi> import Data.Functor.Identity
GHCi> runIdentity $ (getArr2T $ arr2 (+)) 33 9
42
--}
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 = undefined

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 = undefined

-- solution

```
test

### 3.4.4 Functor Reader

Начнем писать реализацию: Функтор, Аппликативный функтор, монада.
Могли бы сразу сделать монаду, получив функтор и аппликатив "бесплатно".
Но в учебных целях пройдем всю цепочку снизу-вверх.
```hs
newtype Reader r a = Reader { runReader :: r -> a } -- было
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } -- стало, m это еще один параметр, внутренняя монада

reader :: (Monad m) => (r -> a) -> ReaderT r m a -- конструктор трансформера
reader f = ReaderT (return . f) -- return after f, ретурн это стандартный способ заворачивания в монаду

-- вспомним основу ридера:
instance Functor ((->) r) where -- базовый механизм утилизации стрелки
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g -- f after g, композиция функций, ибо функтор у нас над стрелкой (функцией из эр)

-- для замаскированной стрелки, функтор отличается не сильно
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f ra = Reader rb where rb = f . (runReader ra) -- вынули внутреннюю стрелку через runReader, сделали композицию f after ra
    fmap f (Reader g) = Reader $ f . g -- тоже самое, но через пат.мат.

runReader (fmap succ $ Reader (*3)) 7 -- 22
```
repl

### 3.4.5 Functor ReaderT

Продолжаем, пишем функтор трансформера
```hs
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f ra = Reader $ f . (runReader ra) -- композиция двух стрелок, завернутая в конструктор Reader
-- продолжаем с этого места

instance (Functor m) => Functor (ReaderT r m) where -- функтор трансформера возможен если внутренняя монада тоже функтор
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f rma = ReaderT (rmb) where rmb = (fmap f) . (runReaderT rma) -- композиция двух стрелок,
    -- с добавкой протаскивания функции через внутреннюю монаду: `fmap f`
    -- протащить функцию через внутр.монаду после распаковки (и применения) стрелки из второго параметра

ghci> :t runReaderT -- подробнее про необходимость fmap внутри реализации fmap
runReaderT :: ReaderT r m a -> r -> m a -- берет ридер rma и возвращает функцию `r -> m a` результатом которой будет монада ma
-- чтобы функцию `a -> b` применить к этому, нужен fmap, реализованный для этой монады (что гарантируется ограничением контекста)

-- примеры работы функтора (fmap)

runReaderT (fmap succ $ reader (*3)) 7 :: [Int] -- [22] -- внутренняя монада: список
runReader (fmap succ $ Reader (*3)) 7 -- 22 -- сравни с ридером не-трансформером

-- рассуждение о том, что конструктор `reader` позволяет использовать внутреннюю монаду только тривиальным образом,
-- в силу применения `return` этой монады.
-- Для не-тривиальных случаев необходимо использовать родной конструктор `ReaderT`
rl3 = ReaderT (\env -> [42, env, env*2]) -- :: ReaderT a [] a -- ReaderListThreeElem = rl3
-- определив стрелку в конструкторе, мы показали компайлеру, что монада будет: список
-- монада списка не-тривиальная, она разветвляет вычисления по трем веткам;

runReaderT (fmap succ rl3) 7 -- [43, 8 15] -- [42+1, 7+1,  (7*2)+1]
```
repl

```hs
https://stepik.org/lesson/38577/step/6?unit=17396
TODO
{--
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов `Functor` в предположении, что `m` является функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
GHCi> (getArr2T $ succ <$> a2l) 10 100
[11,101,111]
GHCi> a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
GHCi> (getArr3T $ sqrt <$> a3e) 2 3 4
Right 3.0
--}
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

-- solution

```
test

### 3.4.7 Applicative Reader

Продолжаем, реализация аппликатива, `Reader`
```hs
-- ранее мы делали аппликатив для стрелки
instance Applicative ((->) r) where
    pure :: a -> (r -> a)
    pure x e = x -- стрелка увеличивает арность, не забываем об этом
    pure x = \e -> x -- через лямбду
    pure x = \_ -> x
    pure = const -- игнор второго параметра, это конст
    (<*>) :: f (a -> b) -> f a -> f b -- оригинальная сигнатура `applied over`
    (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b) -- после подстановки
    (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b -- убрали лишние скобки, увидели: откуда увеличение арности (доп. е)
    (<*>) g h env = g env (h env) -- реализация по сигнатуре: из е получаем а, из а получаем бе
    (<*>) rab ra e = rab e (ra e) -- так понятнее?
    (<*>) g h = \e -> g e (h e) -- через лямбду, в каждое вычисление протаскиваем `e :: r`
-- имея это перед глазами, сделаем аппликатив для ридера

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    rab <*> ra = Reader rb where rb = \env -> runReader rab env (runReader ra env)
-- `runReader rab`, `runReader ra`: распаковка стрелки из контекста ридера, не более того

-- пример использования
runReader (Reader (+) <*> Reader (^2)) 7 -- 56 = 7^2 + 7
```
repl

### 3.4.8 Applicative ReaderT

Реализуем аппликатив для `ReaderT`
```hs
-- сделать аппликатив трансформера можно только если внутренняя монада тоже аппликатив
instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure -- дополнительно запакуем в аппликатив внутренней монады, pure
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    rmab <*> rma = ReaderT rmb where rmb = \env -> (runReaderT rmab env) <*> (runReaderT rma env)
-- внутренний аппликатив делает применение функции `a -> b` внутри контекста внутренней монады
-- Общая идея: там, где раньше был параметр `a`, теперь параметра `m a` с интерфейсом согласно ограничению контекста (Applicative m).

-- example: аппликатив трансформера показывает эффекты как ридера, так и списка (ветвление вычислений):
rl2 = ReaderT (\env -> [const env, (+env)]) -- две функции, ReaderListTwoElem
rl3 = ReaderT (\env -> [42, env, env*2]) -- :: ReaderT a [] a -- ReaderListThreeElem = rl3
runReaderT (rl2 <*> rl3) 7
[7, 7, 7, 49, 14, 21] -- `const env` three times, `+env` three times: (42+7, 7+7, 7*2+7)
-- const env: 42, env, env*2 -- были проигнорированы, ибо const, подставлена 7
-- +env: 42, env, env*2 -> 42+7, 7+7, 7*2+7
```
repl

### 3.4.9 Applicative ReaderT, реализация через liftA2

Композиция двух аппликативных функторов дает аппликативный функтор.
Поэтому, `ReaderT` можно сделать аппликативом просто через композицию внутреннего аппликатива и внешнего.
```hs
-- Реализация через композицию будет использовать liftA2 (подъем двух-параметрической функции в аппликатив),
-- работа заключается в поднятии функции в контекст аппликатива
ghci> :t liftA2 -- функция трех параметров, первый параметр: функция двух параметров
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

ghci> :t (<*>) -- оператор "ап": функция двух параметров, можно использовать в лифте как первый параметр
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

ghci> :t liftA2 (<*>) -- лифтанем "ап" в следующий слой аппликатива
liftA2 (<*>)
  :: (Applicative f1, Applicative f2) => -- получим двух-слойный аппликатив
     f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b) -- получается сигнатура оператора "ап" для `ReaderT`
-- это и есть композиция двух операторов `applied over`, ап внутри и ап снаружи, два слоя ап.

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure -- дополнительно запакуем в аппликатив внутренней монады, pure
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    -- rmab <*> rma = ReaderT rmb where rmb = \env -> (runReaderT rmab env) <*> (runReaderT rma env)
    rmab <*> rma = ReaderT rmb where rmb = liftA2 (<*>) (runReaderT rmab) (runReaderT rma)

-- как видно, вместо вот этого:
\env -> (runReaderT rmab env) <*> (runReaderT rma env)
-- получилось вот это (композиция двух аппликативов):
liftA2 (<*>) (runReaderT rmab) (runReaderT rma)

-- по типам это как-то так:
liftA2 (<*>) ::
f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
~>
(r -> m (a -> b)) -> (r -> m a) -> (r -> m b)
~>
ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b

-- через подстановки, можно доказать, что реализация через композицию (лифт)
-- в точности повторяет оригинальную

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f m1 m2 = f <$> ma <*> m2 -- видно внутренний аппликатив?

f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
~> -- через сигнатуры ReaderT
(r -> m (a -> b)) -> (r -> m a) -> (r -> m b)

g :: r -> m (a -> b)
g = runReaderT rmab
h :: r -> m a
h = runReaderT rma

liftA2 (<*>) g h = -- по определению лифта:
(<*>) <$> g <*> h = -- fmap для аппликатива стрелки = композиция:
((<*>) . g) <*> h = -- развернем оператор "ап" для аппликатива стрелки:
\env -> ((<*>) . g) env (h env) = -- по определению композиции (ап после же):
\env -> (<*>) (g env) (h env) = -- в инфиксном стиле:
\env -> (g env) <*> (h env) = -- развернем обратно:
\env -> (runReaderT rmab env) <*> (runReaderT rma env) = -- чтд

-- для справки:

instance Applicative ((->) r) where -- аппликатив для стрелки, reference
    pure :: a -> (r -> a)
    pure x e = x
    (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
    (<*>) g h = \e -> g e (h e)

instance (Applicative m) => Applicative (ReaderT r m) where -- оригинальная реализация
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    rmab <*> rma = ReaderT rmb where rmb = \env -> (runReaderT rmab env) <*> (runReaderT rma env)
```
repl

```hs
https://stepik.org/lesson/38577/step/10?unit=17396
TODO
{--
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов `Applicative` в предположении, что `m` является аппликативным функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
GHCi> getArr2T (a2fl <*> a2l) 2 10
[22,30,7,7]
GHCi> a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
GHCi> a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]
GHCi> getArr3T (a3fl <*> a3l) 3 5 7
[8,10,10,12]
--}
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

-- solution
-- у проверяющей программе не включено InstanceSigs
-- Тут все равно нужно вписывать свою реализацию функтора

```
test

### 3.4.11 Monad ReaderT

Осталось реализовать интерфейс монады для трансформера `ReaderT`.
Поскольку return = pure, то реализовать надо только оператор `bind`
```hs
-- для справки
instance Monad ((->) r) where
    (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b) -- m >>= k
    m >>= k = \env -> k (m env) env -- env :: r

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b -- m >>= k
    m >>= k = Reader rb where rb = \env -> let v = runReader m env in runReader (k v) env
    -- env :: r; v :: a; k v :: Reader r b

-- поехали:
instance (Monad m) => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b -- m >>= k
    m >>= k = ReaderT rmb where rmb = \env -> do -- do: подняли вычисления во внутреннюю монаду, код 1-в-1 с `Reader`
        v <- runReaderT m env
        runReaderT (k v) env

-- example

rl3 = ReaderT (\env -> [42, env, env*2]) -- :: ReaderT a [] a -- ReaderListThreeElem = rl3
runReaderT (do { x <- rl3; return (succ x) }) 7 -- env = 7
[43, 8, 15] -- env +1 -- два эффекта: ридер (чтение из окружения, 7); список
```
repl

```hs
https://stepik.org/lesson/38577/step/12?unit=17396
TODO
{--
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов `Monad` в предположении, что `m` является монадой:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
[6,8,8,10]
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
Just 81
--}
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

-- solution
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
instance (Functor m, Monad m) => Functor (Arr3T e1 e2 e3 m) where
    fmap = liftM
instance (Applicative m, Monad m) => Applicative (Arr3T e1 e2 e3 m) where
    pure = return
    (<*>) =  ap

```
test

```hs
https://stepik.org/lesson/38577/step/13?unit=17396
TODO
{--
Разработанная нами реализация интерфейса монады для трансформера `Arr3T` (как и для `Arr2T` и `ReaderT`)
имеет не очень хорошую особенность. 
При неудачном сопоставлении с образцом вычисления в этой монаде завершаются аварийно, 
с выводом сообщения об ошибке в диагностический поток:

GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {9 <- a3m; y <- a3m; return y}) 2 3 4
Just 9
GHCi> getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4
*** Exception: Pattern match failure in do expression at :12:15-16

Для обычного ридера такое поведение нормально, 
однако у трансформера внутренняя монада может уметь обрабатывать ошибки более щадащим образом. 
Переопределите функцию `fail` класса типов `Monad` для `Arr3T` так, 
чтобы обработка неудачного сопоставления с образцом осуществлялась бы во внутренней монаде:

GHCi> getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4
Nothing
--}
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

-- solution

```
test

### 3.4.14

Траансформер это монада.

Внутренняя монада может иметь некоторый интерфейс, мы хотим функции этого интерфейса вызывать во внешней монаде.
IMHO: В общем случае это нереально.

Реализуем функцию `lift` для трансформера-монады.

Предисловие - херня, мы хотим функцию, поднимающую любую монаду в трансформер, `lift`
```hs
ghci> :t lift
lift
  :: (MonadTrans t, Monad m) =>
     m a -> t m a

```
repl



grep `TODO` markers, fix it.
