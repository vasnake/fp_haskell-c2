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
{-# HLINT ignore "Eta reduce" #-}

module TestMonads where

import Text.Parsec ( getParserState )
import Text.Read ( readMaybe, readEither, )

import Data.Char ( toLower, toUpper )
import Data.Function ( (&) )
import Data.List ( (++), map, (!!), head, tail, )
import Text.Printf ( printf, )

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
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum,
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when, )
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

add_ :: Int -> Int -> (Int -> r) -> r
add_ x y c = c (x + y)

sumSquares x y c =
    square x $ \x2 ->
    square y $ \y2 ->
    add_ x2 y2 $ \ss -> -- тут мы видим некотурую нелинейность
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


{--
Тип данных для представления ошибки обращения к списку по недопустимому индексу `ListIndexError`
не очень естественно делать представителем класса типов `Monoid`.
Однако, если мы хотим обеспечить накопление информации об ошибках, моноид необходим. 
К счастью, уже знакомая нам функция `withExcept`
позволяет изменять тип ошибки при вычислении в монаде `Except`

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

withExcept :: (e -> e') -> Except e a -> Except e' a

Сделайте тип данных `SimpleError`
представителем необходимых классов типов и 
реализуйте преобразователь для типа данных ошибки `lie2se` так,

newtype SimpleError = Simple { getSimple :: String }
  deriving (Eq, Show)

lie2se :: ListIndexError -> SimpleError 

чтобы обеспечить следующее поведение

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

-- data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)
-- import Text.Printf ( printf )
newtype SimpleError = Simple { getSimple :: String } deriving (Eq, Show)

instance Monoid SimpleError where
    mempty :: SimpleError
    mempty = Simple ""
    mappend :: SimpleError -> SimpleError -> SimpleError
    mappend = (<>)

instance Semigroup SimpleError where
    (<>) :: SimpleError -> SimpleError -> SimpleError
    (Simple e1) <> (Simple e2) = Simple (e1 ++ e2)

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge n) = Simple (printf "[index (%d) is too large]" n)
lie2se ErrNegativeIndex = Simple "[negative index]"

xs = [1, 2, 3]
toSimple = TE.runExcept . TE.withExcept lie2se
toSimpleFromList = TE.runExcept . msum . map (TE.withExcept lie2se)
test16 = toSimple $ xs !!! 42 -- Left (Simple {getSimple = "[index (42) is too large]"})
test17 = toSimple $ xs !!! (-2) -- Left (Simple {getSimple = "[negative index]"})
test18 = toSimple $ xs !!! 2 -- Right 3
test19 = toSimpleFromList [xs !!! (-2), xs !!! 42] -- Left (Simple {getSimple = "[negative index][index (42) is too large]"})
test20 = toSimpleFromList [xs !!! (-2), xs !!! 2] -- Right 3


{--
Стандартная семантика `Except` как аппликативного функтора и монады: 
выполнять цепочку вычислений до первой ошибки. 

Реализация представителей классов `Alternative` и `MonadPlus` наделяет эту монаду альтернативной семантикой: 
попробовать несколько вычислений, вернуть результат первого успешного, а в случае неудачи — все возникшие ошибки.

Довольно часто возникает необходимость сделать нечто среднее.

К примеру, при проверке корректности заполнения анкеты или при компиляции программы для общего успеха необходимо, 
чтобы ошибок совсем не было, но в то же время, нам хотелось бы:
не останавливаться после первой же ошибки, а продолжить проверку,
чтобы отобразить сразу все проблемы.

`Except` такой семантикой не обладает, но никто не может помешать нам сделать свой тип данных 
(назовем его `Validate`), 
представители которого будут обеспечивать требую семантику, позволяющую сохранить список всех произошедших ошибок:

newtype Validate e a = Validate { getValidate :: Either [e] a }

Реализуйте функцию
validateSum :: [String] -> Validate SumError Integer -- список ошибок или сумма

GHCi> getValidate $ validateSum ["10", "20", "30"]
Right 60
GHCi> getValidate $ validateSum ["10", "", "30", "oops"]
Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]

Эта функция практически ничем не отличается от уже реализованной ранее `trySum`, если использовать функцию-адаптер 
collectE :: Except e a -> Validate e a 
и представителей каких-нибудь классов типов для `Validate`

trySum :: [String] -> TE.Except SumError Integer
trySum xs = sum <$> traverse (\(i, s) -> withExcept (SumError i) (tryRead s)) (zip [1..] xs)
-- trySum xs = sum <$> traverse tryRead xs -- начало размышлений, вариант без номеров строк
--}

-- import qualified Control.Monad.Trans.Except as TE
newtype Validate e a = Validate { getValidate :: Either [e] a }

collectE :: TE.Except e a -> Validate e a
collectE ex = Validate (either (Left . (: [])) Right (TE.runExcept ex))
-- Validate (either errF valF (TE.runExcept ex)) where
-- errF e = Left [e]
-- valF v = Right v

validateSum :: [String] -> Validate SumError Integer
validateSum xs = res where
    res = case getValidate errRes of
        Left [] -> sum <$> (sequence foldable) -- no errors
        _ -> errRes
    errRes = msum foldable -- Either [err] x
    foldable = fmap converter (zip [1..] xs) -- list of Either
    converter (i, s) = collectE (numOrErr i s)
    numOrErr i s = TE.withExcept (SumError i) (tryRead s)
-- вот этот выебон не нужен,
-- задачка решается как trySum, через траверс. Код почти идентичный, только ошибку надо конвертнуть

-- validateSum xs = sum <$> traverse (\(i, s) -> collectE (numOrErr i s)) (zip [1..] xs) where
--     numOrErr i s = TE.withExcept (SumError i) (tryRead s)
-- validateSum xs = sum <$> validateOnList where
--     validateOnList = traverse (\(i, s) -> collectE (numOrErr i s)) (zip [1..] xs)
--     numOrErr i s = TE.withExcept (SumError i) (tryRead s)
    -- validateOnList :: Validate SumError [Integer]
    -- validateOnList = mapM (\(i, s) -> collectE (numOrErr i s)) (zip [1..] xs)

{--
нужные операции: <|>, mplus, msum
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
mapM :: Monad m => (a -> m b) -> t a -> m (t b)
traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
either :: (a -> c) -> (b -> c) -> Either a b -> c
--}
instance Functor (Validate e) where
    fmap :: (a -> b) -> Validate e a -> Validate e b
    fmap f v = Validate $ f <$> (getValidate v)

instance Applicative (Validate e) where -- задачка решается через аппликатив, все остальное не надо
    pure :: a -> Validate e a
    pure x = Validate (Right x)
    -- семантика Either, первая встреченная ошибка
    (<*>) :: Validate e (a -> b) -> Validate e a -> Validate e b
    fs <*> xs = Validate ((getValidate fs) <*> (getValidate xs))

instance Monad (Validate e) where
    return :: a -> Validate e a
    return = pure
    (>>=) :: Validate e a -> (a -> Validate e b) -> Validate e b
    m >>= k = either (Validate . Left) k (getValidate m)

instance Alternative (Validate e) where
    empty :: Validate e a
    empty = Validate (Left empty)

    -- ok, ok -> ok
    -- err, err -> sum
    -- err, ok -> err
    -- ok, err -> err
    (<|>) :: Validate e a -> Validate e a -> Validate e a
    v1 <|> v2 = Validate v3 where
        v3 = case (getValidate v1, getValidate v2) of
            (Left x, Left y) -> Left (x `mappend` y)
            (Left x, _) -> Left x
            (Right x, Left y) -> Left y
            (Right x, Right y) -> Right y

instance MonadPlus (Validate e) where
    mzero :: Validate e a
    mzero = empty
    mplus :: Validate e a -> Validate e a -> Validate e a
    mplus = (<|>)

test21 = getValidate $ validateSum ["10", "20", "30"] -- Right 60
test22 = getValidate $ validateSum ["10", "", "30", "oops"] -- Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]


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
decode f = f 0
as n f = f n
a n f = f n
number = id

one n f = f (n + 1)
two n f = f (n + 2)
three n f = f (n + 3)
seventeen n f = f (n + 17)
twenty n f = f (n + 20)
hundred n f = f (n * 100)
thousand n f = f (n * 1000)

test23 = decode one hundred twenty three as a number -- 123
test24 = decode one hundred twenty one as a number -- 121
test25 = decode one hundred twenty as a number -- 120
test26 = decode one hundred as a number -- 100
test27 = decode three hundred as a number -- 300
test28 = decode two thousand seventeen as a number -- 2017


{--
Реализуйте функцию `showCont`, запускающую вычисление и возвращающую его результат в виде строки.
--}
showCont :: Show a => Cont String a -> String
showCont c = runCont c show


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

> ... анализирует и модифицирует? значение, возвращаемое кодом, написанным после ...

(Если ни возвращенное, ни сохраненные значения не подходят, 
результатом должно быть первое из сохраненных значений; 
если не было сохранено ни одного значения, то результатом должно быть возвращенное значение.)

Обратите внимание на то, что функция `checkpoint` передается в `Checkpointed` вычисление как параметр, 
поскольку её поведение зависит от предиката, который будет известен только непосредственно при запуске.
--}

-- type Checkpointed a = (a -> [a]) -> [a]
-- Cont resultType valueType -- runCont :: (a -> r) -> r
-- type Checkpointed a = (a -> Cont [a] a) -> Cont [a] a
type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed predicate checkpointed = runCont xz id where
    xz = checkpointed foo
    -- некая функция, которая проверяет значение и либо продолжает конт. либо рвет его
    -- foo = (\x -> if predicate x then ok x else stop x) -- foo :: a -> Cont a a
    -- ok = return -- ok :: Cont a a
    foo x = Cont (\next -> if predicate $ next x then next x else x )

{--
test2 :: Integer -> Cont r Integer
test2 x = callCC (\k -> do
    a <- return 3
    when (x > 100) (k 42) -- при выполнении условия здесь цепочка прерывается
    -- иначе игнор выражения и переход на следующее
    return (a + x))

--}

-- end of solution

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
    checkpoint x1
    let x2 = x1 + 10
    checkpoint x2     {- x2 = x1 + 10 -}
    let x3 = x2 + 10
    checkpoint x3     {- x3 = x1 + 20 -}
    let x4 = x3 + 10
    return x4         {- x4 = x1 + 30 -}

test29 = runCheckpointed (< 100) $ addTens 1 -- 31
test30 = runCheckpointed  (< 30) $ addTens 1 -- 21
test31 = runCheckpointed  (< 20) $ addTens 1 -- 11
test32 = runCheckpointed  (< 10) $ addTens 1 -- 1


{--
Вычисление в монаде `Cont` передает результат своей работы в функцию-продолжение. 
А что, если наши вычисления могут завершиться с ошибкой? 
В этом случае мы могли бы явно возвращать значение типа `Either` и 
каждый раз обрабатывать два возможных исхода, что не слишком удобно. 
Более разумный способ решения этой проблемы предоставляют трансформеры монад, но с ними мы познакомимся немного позже.

Определите тип данных `FailCont` для вычислений, которые 
получают два продолжения и 
вызывают одно из них в случае успеха, а другое — при неудаче. 

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

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
evalCont m = runCont m id -- evalCont :: Cont r r -> r
instance Monad (Cont r) where
    return :: a -> Cont r a
    return x = Cont (\c -> c x)
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    (Cont v) >>= k = Cont (\c -> v (\a -> runCont (k a) c)) -- bind v k c = v (\a -> k a c)
--}
-- import qualified Control.Monad.Trans.Except as TE
-- import Control.Monad
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: TE.Except e a -> FailCont r e a
toFailCont ex = FailCont (\ vf ef -> either ef vf (TE.runExcept ex))

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Right Left

instance Monad (FailCont r e) where
    return :: a -> FailCont r e a
    return x = FailCont (\ vf ef -> vf x)

    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    -- ve, k: value-error, Kleisli (arrow)
    -- vc, ec: value cont., error cont.
    -- v, e: value, error
    (FailCont ve) >>= k = FailCont (\ vc ec -> ve -- получаем два конт., запускаем левую монаду, внутри два продолжения:
        (\v -> runFailCont (k v) vc ec) -- обработка значения, цепочка с правым параметром (Клейсли)
        (\e -> ec e) -- обработка ошибки
        )

-- fmap = liftM, pure = return, (<*>) = ap
instance Functor (FailCont r e) where
    fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
    fmap = liftM

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a
    pure = return
    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    (<*>) = ap

-- end of solution

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

test33 = evalFailCont $ addInts "15" "12" -- Right 27
test34 = runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show) -- Oops: EmptyInput


{--
Реализуйте функцию `callCFC` для монады `FailCont` по аналогии с `callCC`

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont (\c -> -- `c` это передаваемый снаружи "терминатор" (или следующее продолжение)
    runCont (f (\k -> Cont (\_ -> c k))) c
    ) -- f это наша лямбда, где у нас две ветки:
    -- либо дергаем k (выход из конт.), либо не дергаем (продолжаем цепочку)
    -- когда дергаем: игнорим дальнейший конт, что закрывает цепочку.

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }
--}

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
-- callCFC f = FailCont (\ ok err ->
--     runFailCont (f 
--         (\k -> FailCont (\ _ _ -> ok k)) -- рвем цепочку, игнорим продолжения
--         ) ok err)
callCFC f = FailCont $ \c -> 
    runFailCont (f $ \a -> FailCont $ \_ _ -> c a) c
