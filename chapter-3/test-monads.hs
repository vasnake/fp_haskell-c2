{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

{-# HLINT ignore "Use traverse_" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Using foldr on tuple" #-}
{-# HLINT ignore "Using maximum on tuple" #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module TestMonads where

import Text.Parsec ( getParserState )
import Text.Read ( readMaybe, readEither, )

import Data.Char ( toLower, toUpper )
import Data.Function ( (&) )
import Data.List ( (++), map, (!!), head, tail, )

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..)
    )

import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor ( (<&>) )

import Control.Applicative (
    Alternative(..), Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>)
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when )
-- import Control.Monad.Cont ( callCC )
-- import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
-- import Control.Applicative (Alternative(..))

-- import Control.Monad.Trans.Reader as TR hiding (reader, Reader, ReaderT, runReader, runReaderT )
-- import Control.Monad.Trans.Reader as TR ( asks)
 -- ( ask, asks, Reader(..), ReaderT(..) )

import Control.Monad.Trans.Writer ( runWriter, tell, Writer )
-- import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as TE

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take, Monad(..),
    Double, either, Integer, head, tail, IO(..), Read(..), ReadS(..), read, reads,
    zip,
    )

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

instance Monad (Except e) where
    return a = Except (Right a) -- упаковка значения в контекст
    m >>= k = case runExcept m of
        Left e -> Except (Left e) -- была ошибка, продвигаем ее далее
        Right a -> k a -- k: Kleisli -- ошибки нет, работаем дальше

throwE :: e -> Except e a
throwE = except . Left

-- версия bind для обработки ошибки (функция над ошибкой, в отличие от bind, где функция над значением)
catchE :: Except e a -> (e -> Except e2 a) -> Except e2 a
m `catchE` h = case runExcept m of
    Left e -> h e
    Right a -> except (Right a)

data DivByError = ErrZero String | ErrOther deriving (Eq, Show)

(/?) :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
    action = do
        q <- x /? y
        return $ show q
    -- handler = \err -> return $ show err
    handler = return . show

instance (Monoid e) => Alternative (Except e) where
    empty = mzero
    (<|>) = mplus

-- вот самое интересное, моноидальное накапливание ошибок и поддержка альтернативных вычислений,
-- если предыдущие (в пайплайне) сломались
instance (Monoid e) => MonadPlus (Except e) where
    mzero = Except (Left mempty) -- throwE mempty -- создается ошибка на нейтральном содержимом

    -- склейка двух ошибок или проброс результата
    (Except x) `mplus` (Except y) = Except $
        case x of
            Left e -> either (Left . mappend e) Right y -- если левый шаг пайплайна содержит ошибку
            -- результат определяется содержимым правого шага.
            -- три аргумента у функции either, функция из `y :: Either e a`
            -- сделает либо ошибку, либо результат (в зависимости от содержимого y)
            -- если в у ошибка, то две ошибки суммируются в моноиде,
            -- если в у результат, то он прокидывается дальше (см. ветку `r -> r`)
            r -> r -- если левый шаг пайплайна без ошибки, он дальше и пойдет

    -- x `mplus` y = withExcept mappend x `catchE` flip withExcept y

instance Monoid DivByError where
    mempty = ErrOther
    ErrOther `mappend` ErrOther = ErrOther
    ErrOther `mappend` (ErrZero s2) = ErrZero s2
    (ErrZero s1) `mappend` ErrOther = ErrZero s1
    (ErrZero s1) `mappend` (ErrZero s2) = ErrZero $ s1 ++ s2

instance Semigroup DivByError where -- required by class hierarchy
    (<>) = mappend

-- стало
example2 :: Double -> Double -> Except DivByError String
example2 x y = action `catchE` handler where
    action = do
        q <- x /? y
        guard (y >= 0) -- результат использования альтернативы, вернет mempty на условии `y < 0` а mempty = ErrOther
        return (show q) -- нет ошибки
    handler (ErrZero s) = return s
    handler ErrOther = return ("negative y: " ++ show y)
-- сравните с предыдущим вариантом:
{--
example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
    action = do
        q <- x /? y
        return $ show q
    -- handler = \err -> return $ show err
    handler = return . show
--}

test1 = runExcept $ msum [5 /? 0, 7 /? 0, 2 /? 0]

square :: Int -> (Int -> r) -> r
square x c = c (x ^ 2)

add :: Int -> Int -> (Int -> r) -> r
add x y c = c (x + y)

sumSquares x y c =
    square x $ \x2 ->
    square y $ \y2 ->
    add x2 y2 $ \ss -> -- тут мы видим некотурую нелинейность
    c ss
-- напоминает do-нотацию монады, не так ли?

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
evalCont :: Cont r r -> r -- удобный враппер терминирования конт. на функции id (гарантирует совпадение типа а с типом эр)
evalCont m = runCont m id

instance Functor (Cont r) where
    fmap = liftM

instance Applicative (Cont r) where
    pure = return
    (<*>) = ap

instance Monad (Cont r) where
    return :: a -> Cont r a
    return x = Cont (\c -> c x) -- монада над стрелочным типом, поэтому лямбда

    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b -- m bind k: монада бинд стрелкаКлейсли, на выходе монада
    (Cont v) >>= k = Cont (\c -> v (\a -> runCont (k a) c)) -- лямбды, ибо работаем со стрелочным типом
    -- нам должны дать конт. си, тогда мы запустим вычисление `v (...)`
    -- в котором нам должны дать аргумент для стрелкаКлейсли

sumIt :: Cont r Integer
sumIt = do
    a <- return 3
    b <- return 5
    return (a + b)

test :: Integer -> Cont Integer Integer
test x = do
    a <- return 3
    -- вместо `return ()` вмешаемся в вызов продолжения:
    Cont (\c -> if x > 100 then 42 else c ()) -- выбор ветки исполнения следующего шага, в зависимости от аргумента
    -- если х больше 100 то выходим, не вывываем продолжение, иначе вызываем `c ()`, где юнит это просто заглушка   
    -- прерывание вычислений по условию
    return (a + x)
-- 42 портит нам полиморфизм, фиксируя параметр r в `Cont r a`
-- callCC решает эту проблему

test2 :: Integer -> Cont r Integer
test2 x = callCC (\k -> do
    a <- return 3
    when (x > 100) (k 42) -- при выполнении условия здесь цепочка прерывается
    -- иначе игнор строки и переход на следующую
    return (a + x))
-- k :: Integer -> Cont r b

-- callCC :: ((a -> m b) -> m a) -> m a 
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont (\c -> runCont (f (\a -> Cont (\_ -> c a))) c)

-- будем решать задачу объединения ридера и врайтера
-- эффект ридера, читает второй элемент из списка (строк    )
{--
secondElem :: Reader [String] String
secondElem = do
    el2 <- asks ((map toUpper) . head . tail)
    return el2
--}

strings = ["ab", "cd", "fg"]

logFirst :: [String] -> Writer String String
logFirst xs = do
    let el1 = head xs
    let el2 = ((map toUpper) . head . tail) xs
    tell el1
    return el2
-- runWriter (logFirst strings)

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

type MyRW = ReaderT [String] (Writer String)

logFirstAndRetSecond' :: MyRW String
logFirstAndRetSecond' = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift (tell el1) -- подъем из внутренней монады -- можно читать как "поднять API внутренней монады"
    return el2

-- утилита запуска
runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)
-- runMyRW logFirstAndRetSecond strings


newtype Reader r a = Reader { runReader :: r -> a } -- было
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } -- стало, m это еще один параметр, внутренняя монада

-- Ридер это функциональная стрелка, со связанным первым параметром.
-- Абстракция "чтения из окружения".

-- reader :: (Monad m) => (r -> a) -> ReaderT r m a -- конструктор
-- Нам нужен кастомный конструктор, чтобы иметь возможность упаковать стрелку `r -> a` в композитную монаду.
-- Если этого не сделать, конструктор `ReaderT` будет требовать стрелку в монаду `r -> m a`,
-- это не то, что мы хотим

t0 = runReader (Reader (*3)) 7 -- стрелка пакуется в монаду ридер, ок
-- runReaderT (ReaderT (func)) 7 where func = \x -> [x*3] -- для трансформера было бы так, внутренняя монада: список

reader :: (Monad m) => (r -> a) -> ReaderT r m a -- реализация
reader f = ReaderT (return . f) -- return after f, ретурн это стандартный способ заворачивания в монаду

-- t1 = runReaderT (reader (*3)) 7 -- выражение полиморфно по внутренней монаде, надо указать компайлеру, какая монада нам нужна
t2 = runReaderT (reader (*3)) 7 :: [Int] -- внутренняя монада: список
t3 = runReaderT (reader (*3)) 7 :: Maybe Int
t4 = runReaderT (reader (*3)) 7 :: IO Int

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f ra = Reader rb where rb = f . (runReader ra) -- вынули внутреннюю стрелку через runReader, сделали композицию f after ra
    -- fmap f (Reader g) = Reader $ f . g -- тоже самое, но через пат.мат.

t5 = runReader (fmap succ $ Reader (*3)) 7 -- 22

instance (Functor m) => Functor (ReaderT r m) where -- функтор трансформера возможен если внутренняя монада тоже функтор
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f rma = ReaderT (rmb) where rmb = (fmap f) . (runReaderT rma) -- композиция двух стрелок,
    -- с добавкой протаскивания функции через внутреннюю монаду: `fmap f`
    -- протащить функцию через внутр.монаду после распаковки (и применения) стрелки из второго параметра

-- примеры работы функтора (fmap)
t6 = runReaderT (fmap succ $ reader (*3)) 7 :: [Int] -- [22] -- внутренняя монада: список
t7 = runReader (fmap succ $ Reader (*3)) 7 -- 22 -- сравни с ридером не-трансформером

-- рассуждение о том, что конструктор `reader` позволяет использовать внутреннюю монаду только тривиальным образом,
-- в силу применения `return` этой монады.
-- Для не-тривиальных случаев необходимо использовать родной конструктор `ReaderT`
rl3 = ReaderT (\env -> [42, env, env*2]) -- :: ReaderT a [] a -- ReaderListThreeElem = rl3
-- определив стрелку в конструкторе, мы показали компайлеру, что монада будет: список
-- монада списка не-тривиальная, она разветвляет вычисления по трем веткам;

t8 = runReaderT (fmap succ rl3) 7 -- [43, 8 15] -- [42+1, 7+1,  (7*2)+1]

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    rab <*> ra = Reader rb where rb env = runReader rab env (runReader ra env)
    -- rab <*> ra = Reader rb where rb = \env -> runReader rab env (runReader ra env)
-- `runReader rab`, `runReader ra`: распаковка стрелки из контекста ридера, не более того

-- пример аппликатива
t9 = runReader (Reader (+) <*> Reader (^2)) 7 -- 56 = 7^2 + 7

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure -- дополнительно запакуем в аппликатив внутренней монады, pure
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    rmab <*> rma = ReaderT rmb where rmb = liftA2 (<*>) (runReaderT rmab) (runReaderT rma) -- реализация через лифт (композиция двух аппликативов)
    -- rmab <*> rma = ReaderT rmb where rmb env = (runReaderT rmab env) <*> (runReaderT rma env)
    -- rmab <*> rma = ReaderT rmb where rmb = \env -> (runReaderT rmab env) <*> (runReaderT rma env)
-- внутренний аппликатив делает применение функции `a -> b` внутри контекста внутренней монады
-- Общая идея: там, где раньше был параметр `a`, теперь параметра `m a` с интерфейсом согласно ограничению контекста (Applicative m).

rl2 = ReaderT (\env -> [const env, (+env)]) -- две функции, ReaderListTwoElem
-- rl3 = ReaderT (\env -> [42, env, env*2]) -- :: ReaderT a [] a -- ReaderListThreeElem = rl3
t10 = runReaderT (rl2 <*> rl3) 7

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b -- m >>= k
    m >>= k = Reader rb where rb env = let v = runReader m env in runReader (k v) env
    -- m >>= k = Reader rb where rb = \env -> let v = runReader m env in runReader (k v) env
    -- env :: r; v :: a; k v :: Reader r b

instance (Monad m) => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b -- m >>= k
    m >>= k = ReaderT rmb where
        rmb env = do -- do: подняли вычисления во внутреннюю монаду, код 1-в-1 с `Reader`         
            v <- runReaderT m env
            runReaderT (k v) env

instance MonadTrans (ReaderT r) where
    lift :: (Monad m) => m a -> ReaderT r m a
    lift m = ReaderT (\_ -> m) -- ридер это стрелочный тип, поэтому лямбда

ask :: (Monad m) => ReaderT r m r -- для трансформера стало так
ask = ReaderT return

asks :: (Monad m) => (r -> a) -> ReaderT r m a -- для трансформера стало так
asks f = ReaderT (return . f) -- внутренняя монада требует наличия `return`

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f rma = ReaderT ((runReaderT rma) . f) -- так как ридер это функция, имееем композицию функций
-- следует помнить, енв прокидывается в разные вычисления независимо (см. пример)


{--
Реализуйте функцию 

withExcept :: (e -> e') -> Except e a -> Except e' a

позволящую, если произошла ошибка, применить к ней заданное преобразование.
--}
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except (Left e)) = except $ Left (f e)
withExcept _ (Except (Right a)) = except $ Right a

-- newtype Except e a = Except { runExcept :: Either e a } deriving Show
-- except :: Either e a -> Except e a
-- except = Except



{--
В модуле `Control.Monad.Trans.Except` библиотеки `transformers` 
имеется реализация монады `Except` 
с интерфейсом, идентичным представленному в видео-степах, но с более общими типами. 
Мы изучим эти типы в следующих модулях, однако 
использовать монаду `Except` из библиотеки `transformers` мы можем уже сейчас.

Введём тип данных для представления ошибки обращения к списку по недопустимому индексу:

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)

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

-- import qualified Control.Monad.Trans.Except as TE
data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)
infixl 9 !!!
(!!!) :: [a] -> Int -> TE.Except ListIndexError a
(!!!) lst idx
    | idx < 0 = TE.except (Left ErrNegativeIndex)
    | otherwise = case (lst !? idx) of
        Just x -> TE.except (Right $ lst !! idx)
        Nothing -> TE.except (Left $ ErrIndexTooLarge idx)
-- | idx >= (length lst) = TE.except (Left $ ErrIndexTooLarge idx) -- no infinity here

(!?) :: (Ord a, Num a, Foldable t) => t b -> a -> Maybe b
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

(!!!!) xs n = TE.runExcept $ xs !!! n
test3 = TE.runExcept $ [1..100] !!! 5 -- Right 6
test4 = [1,2,3] !!!! 0 -- Right 1
test5 = [1,2,3] !!!! 42 -- Left (ErrIndexTooLarge 42)
test6 = [1,2,3] !!!! (-3) -- Left ErrNegativeIndex


{--
Реализуйте функцию `tryRead`, 
получающую на вход строку и пытающуюся всю эту строку превратить в значение заданного типа. 
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

-- import qualified Control.Monad.Trans.Except as TE
-- import Prelude ( Read, ReadS(..), read, reads, )
-- import Text.Read (readMaybe, readEither, )
data ReadError = EmptyInput | NoParse String
    deriving Show

tryRead :: (Read a)=> String -> TE.Except ReadError a
tryRead "" = TE.throwE EmptyInput
tryRead s = case parse s of
    Nothing -> TE.throwE $ NoParse s
    Just x -> pure x

parse :: (Read a)=> String -> Maybe a
parse = readMaybe

test7 = TE.runExcept (tryRead "5" :: TE.Except ReadError Int) -- Right 5
test8 = TE.runExcept (tryRead "5" :: TE.Except ReadError Double) -- Right 5.0
test10 = TE.runExcept (tryRead "(True, ())" :: TE.Except ReadError (Bool, ())) -- Right (True,())
test11 = TE.runExcept (tryRead "" :: TE.Except ReadError (Bool, ())) -- Left EmptyInput
test9 = TE.runExcept (tryRead "5zzz" :: TE.Except ReadError Int) -- Left (NoParse "5zzz")
test12 = TE.runExcept (tryRead "wrong" :: TE.Except ReadError (Bool, ())) -- Left (NoParse "wrong")


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

-- import qualified Control.Monad.Trans.Except as TE
data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> TE.Except SumError Integer
trySum xs = foldr f zero (zip [1 ..] xs) where
    zero = pure 0
    f (i, s) ex = sumOrErr where
        sumOrErr = do
            x1 <- (tryRead s :: TE.Except ReadError Integer) `TE.catchE` errConv -- left error first
            x2 <- ex
            return (x1 + x2)
        errConv = \readErr -> TE.throwE (SumError i readErr)

instance Monoid SumError where
    mempty = undefined
    mappend = undefined

instance Semigroup SumError where
    (<>) = mappend

{--
foldr возвращает самую правую ошибку, надо самую левую (но надо работать с бесконечностью)
trySum :: [String] -> TE.Except SumError Integer
trySum xs = foldr f zero (zip [1 ..] xs) where
    zero = pure 0
    f (i, s) ex = case TE.runExcept ex of
        Left _ -> ex -- (SumError idx readErr)
        Right x1 -> sumOrErr `TE.catchE` errConv where
            sumOrErr = do
                x2 <- tryRead s :: TE.Except ReadError Integer
                return (x1 + x2)
            errConv = (\readErr -> TE.throwE (SumError i readErr))

--}

test13 = TE.runExcept $ trySum ["10", "20", "30"] -- Right 60
test14 = TE.runExcept $ trySum ["10", "20", ""] -- Left (SumError 3 EmptyInput)
test15 = TE.runExcept $ trySum ["10", "two", "30"] -- Left (SumError 2 (NoParse "two"))

tryReadInt :: String -> TE.Except ReadError Int
tryReadInt "" = TE.throwE EmptyInput
tryReadInt s = case readMaybe s of
    Nothing -> TE.throwE $ NoParse s
    Just x -> pure x
