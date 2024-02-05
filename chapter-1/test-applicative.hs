{-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
{-# LANGUAGE PolyKinds #-}

module TestApplicative where
import Prelude (show, read, String, Char, Functor, fmap, Bool, otherwise, Int, (==), (*), id, const, Maybe (..), null, ($), succ, (.), undefined, Num ((+)))
import Text.Parsec (getParserState)
import Data.Char
import Control.Applicative hiding (many)
import GHC.Show (Show)
import GHC.Base (Eq)

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

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure
    (<*>) = undefined

test6 = getCmps $ Cmps [Just (+1), Just (+2)] <*> Cmps [Just 30, Just 40]
test7 = getCmps $ Cmps (Just [(+1), (+2)]) <*> Cmps (Just [30, 40])
