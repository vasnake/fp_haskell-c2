{-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Using <$> on tuple" #-}

module TestApplicative where

import Prelude (
    show, read, String, Char, Functor, fmap, Bool, otherwise, Int,
    (==), (*), (^), (-), (/), (++), id, const, Maybe (..), null, ($), succ, (.), undefined, Num ((+)),
    length, take, tail, zipWith, Fractional
    )

import Text.Parsec (getParserState)

import Data.Char ( Char, digitToInt, isDigit, isLower )

import Control.Applicative (
    Applicative((<*>), pure, (<*)), (<$>),
    Alternative((<|>), empty),
    ZipList(ZipList), getZipList
    )

import GHC.Show ( Show(..) )
import GHC.Base ( Eq(..) )

newtype Parser a = Parser { apply :: String -> [(a, String)] }

anyChar :: Parser Char -- примитивный парсер №1
anyChar = Parser f where -- парсер это стрелка из строки в список пар; пара содержит распарсенное значение и остаток строки
    f "" = []
    f (c:cs) = [(c, cs)]

-- реализуем интерфейс функтора
instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b -- fmap or <$>
    fmap :: (a -> b) -> Parser a -> Parser b -- fmap or <$>
    fmap f p = Parser fun where -- парсер `p` это функция (ака стрелка); `apply p` дает нам функцию из строки в список
        fun s = [ (f a, s2) | (a, s2) <- apply p s ]
-- что значит "поднять функцию в функтор" в контексте парсера?
-- это значит: применить данную функцию ко всем значениям (голова пары) из списка,
-- полученного применением данного парсера к той строке, на которой это будет работать (параметр `s`)

-- пример функтора
test = apply (digitToInt <$> anyChar) "123abc"

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

-- пример цепочки
test2 = apply ((,) <$> anyChar <*> anyChar) "abcde"

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
    f ""                    = []
    f (c:cs)    | pr c      = [(c, cs)]
                | otherwise = []

lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit -- умножить две цифры, разделенные *, сама звезда игнор.

test3 = Nothing <|> Just 3 <|> Just 5 <|> Nothing

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

test4 = apply (char 'A' <|> char 'B') "ABC"

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show) -- design time wrapper
-- `f (g a)` это два конструктора типов, примененные к а.
-- kind a = *
-- kind g = * -> *
-- kind f = * -> *

-- f, g оба функторы, иначе не работает, ибо делаем композицию функторов
instance (Functor f, Functor g) => Functor (f |.| g) where
    -- fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b -- сигнатура, функция -> функтор -> функтор
    -- fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b -- то же, в префиксной форме
    -- fmap h (Cmps x) = _ -- надо протащить h через f, g
-- имеем x :: f (g a)
-- h :: a -> b
-- f, g это функторы, протаскивание функции делается через fmap
-- допустим, мы сделаем функцию phi :: g a -> g b
-- тогда (fmap phi x) :: f (g b)
-- что такое phi? Это `(fmap h) :: g a -> g b` -- для любого функтора g
    -- fmap h (Cmps x) = Cmps fgb where
    --     fgb = fmap phi x where
    --         phi = fmap h
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

test5 = fmap succ $ Cmps (Just "abc")
-- test5 = succ <$> Cmps (Just "abc")

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- (<*>) :: (|.|) f g (a -> b) -> (|.|) f g a -> (|.|) f g b -- в развернутом виде
    Cmps h <*> Cmps x = Cmps $ (fmap (<*>) h) <*> x
    -- Cmps h <*> Cmps x = Cmps $ fmap (<*>) h <*> x

test6 = getCmps $ Cmps [Just (+1), Just (+2)] <*> Cmps [Just 30, Just 40]
test7 = getCmps $ Cmps (Just [(+1), (+2)]) <*> Cmps (Just [30, 40])


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
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b -- (a -> b) -> f a -> f b
  fmap f arr2 = Arr2 arr2' where
    arr2' e1 e2 = f (getArr2 arr2 e1 e2)
    -- e1e2b = \ e1 e2 -> ab (getArr2 e1e2a e1 e2)
    -- e1e2b e1 = ab . (getArr2 e1e2a e1)

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b -- (a -> b) -> f a -> f b
  fmap f arr3 = Arr3 arr3' where
    arr3' e1 e2 e3 = f (getArr3 arr3 e1 e2 e3)

test8 = getArr2 (fmap length (Arr2 take)) 10 "abc" -- 3
test9 = getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50] -- [33,44]


{--
Следующий тип данных задает гомогенную тройку элементов, которую можно рассматривать как трехмерный вектор

data Triple a = Tr a a a  deriving (Eq, Show)

Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой покоординатного применения

GHCi> (^2) <$> Tr 1 (-2) 3
Tr 1 4 9
GHCi> Tr (^2) (+2) (*3) <*> Tr 2 3 4
Tr 4 5 12
--}
data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure :: a -> Triple a
    pure x = Tr x x x

    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (Tr fx fy fz) <*> (Tr x y z) = Tr (fx x) (fy y) (fz z)

test10 = (^2) <$> Tr 1 (-2) 3 -- Tr 1 4 9
test11 = Tr (^2) (+2) (*3) <*> Tr 2 3 4 -- Tr 4 5 12


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
-- import Control.Applicative (ZipList(ZipList), getZipList)
x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

-- (>$<) :: (a -> b) -> [a] -> [b]
-- (>$<) = (<$>) -- нельзя, ZipList != []
(>$<) f xs = getZipList (f <$> (ZipList xs))

-- (>*<) :: [a -> b] -> [a] -> [b]
-- (>*<) = (<*>) -- нельзя, ZipList != []
(>*<) xs ys = getZipList ((ZipList xs) <*> (ZipList ys))

-- getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
test12 = (\a b -> 2*a+3*b) >$< x1s >*< x2s -- [14,19,24]

test13 = (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s -- [49,59,69]
test14 = (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s -- [9,15,21]


{--
Функция

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

сворачивает список посредством деления. Модифицируйте ее, реализовав
divideList' :: (Show a, Fractional a) => [a] -> (String, a)
такую что последовательность вычислений отражается в логе

GHCi> divideList [3,4,5]
3.75
GHCi> divideList' [3,4,5]
("<-3.0/<-4.0/<-5.0/1.0",3.75)

Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации
--}
divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = divOp <$> firstPair <*> secondPair where -- (/) <$> ("<-" ++ (show x) ++ "/", x) <*> (divideList' xs)
    divOp = (/)
    firstPair = (log x, x)
    secondPair = (divideList' xs)
    log a = "<-" ++ (show a) ++ "/"

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

test15 = divideList [3,4,5] -- 3.75
test16 = divideList' [3,4,5] -- ("<-3.0/<-4.0/<-5.0/1.0",3.75)


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
{--
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b -- (a -> b) -> f a -> f b
  fmap f arr2 = Arr2 arr2' where
    arr2' e1 e2 = f (getArr2 arr2 e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b -- (a -> b) -> f a -> f b
  fmap f arr3 = Arr3 arr3' where
    arr3' e1 e2 e3 = f (getArr3 arr3 e1 e2 e3)
--}
instance Applicative (Arr2 e1 e2) where
  pure :: a -> Arr2 e1 e2 a -- a -> f a
  pure = undefined
  (<*>) :: Arr2 e1 e2 (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b -- f (a -> b) -> f a -> f b
  (<*>) = undefined

instance Applicative (Arr3 e1 e2 e3) where
  pure :: a -> Arr3 e1 e2 e3 a -- a -> f a
  pure = undefined
  (<*>) :: Arr3 e1 e2 e3 (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b -- f (a -> b) -> f a -> f b
  (<*>) = undefined
{--
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
--}

test17 = getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3 -- -1
test18 = getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4 -- -15
