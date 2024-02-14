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



grep `TODO` markers, fix it.
