{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

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
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}

module TestTransformers where

import Text.Parsec ( getParserState )
import Text.Read ( readMaybe, readEither, )
import Data.Char ( toLower, toUpper, isNumber, isPunctuation, )
import Data.Function ( (&) )
import Data.Either ( lefts, isLeft, )
import Data.Bool

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..)
    )

import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor ( (<&>) )

import Control.Applicative (
    Alternative(..), Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>), liftA2,
    Const(..),
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when, MonadFail )
import Control.Monad.Trans ( MonadTrans(..), MonadIO(..), )
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Cont ( callCC )
-- import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
-- import Control.Applicative (Alternative(..))

import Control.Monad.Trans.Class
-- import Control.Monad.Trans ( lift )
import qualified Control.Monad.Trans.Writer as TW
import qualified Control.Monad.Trans.Reader as TR
import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.Trans.State as TS
import qualified Control.Monad.Trans.Maybe as TM
-- import Control.Monad.Trans.Writer ( runWriter, tell, Writer )
-- import Control.Monad.Trans.Reader as TR hiding (reader, Reader, ReaderT, runReader, runReaderT )
-- import Control.Monad.Trans.Reader as TR ( asks) -- ( ask, asks, Reader(..), ReaderT(..) )
import qualified Control.Monad.Except as E

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, pred, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take, Monad(..),
    Double, either, Integer, head, tail, IO(..), snd, pi, fromIntegral,
    repeat, fst, snd, (&&), filter, Bool(..), replicate, concatMap, getLine, any, all,
    Read(..), Integral(..),
    )

import Debug.Trace ( trace, )
debug = flip trace

-- newtype Writer w a = Writer { runWriter :: (a, w) } -- было
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) } -- будем реализовывать

t1 = runWriterT (WriterT (Just (42, "foo"))) -- внутренняя монада Maybe

writer :: (Monad m) => (a, w) -> WriterT w m a
writer = WriterT . return -- упаковали во внутреннюю монаду (полиморфизм), упаковали во врайтер

t2 = runWriterT (writer (42, "foo")) :: [(Int, String)] -- сняли полиморфизм, явно указав тип внутренней монады

execWriterT :: (Monad m) => WriterT w m a -> m w
execWriterT = (fmap snd) . runWriterT -- монада лога из монады пары получается через функтор (fmap)

instance (Functor m) => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . (fmap updater) . runWriterT
        where updater ~(x, log) = (f x, log) -- ленивый пат.мат, неопровержимый образец

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT (pure (x, mempty)) -- вызов пюре для внутренней монады (аппликатива)

    -- liftA2 поднимает создание новой пары во внутреннюю монаду (аппликатив)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v)
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= k = WriterT $ do -- ду-нотация залезает во внутреннюю монаду, внешнюю оболочку снимают метки доступа `runWriterT`
        ~(v1, w1) <- runWriterT m -- левая часть, лениво
        ~(v2, w2) <- runWriterT (k v1) -- правая часть, лениво
        return (v2, w1 `mappend` w2) -- упаковали во внутреннюю монаду
    -- fail :: String -> WriterT w m a -- вопрос, что делать при ошибках? ХЗ, зависит от семантики
    -- fail = WriterT . fail -- делегируем обработку ошибки во внутреннюю монаду (если это спиисок, то будет эффект прерывания цикла)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift :: (Monad m) => m a -> WriterT w m a
    lift m = WriterT $ do
        x <- m -- вынули из пришедшей монады
        return (x, mempty) -- засунули во внутреннюю монаду, добавив пустой лог

tell :: (Monad m) => w -> WriterT w m ()
tell w = writer ((), w)

-- по аналогии с юниксовым `tee`, выдать лог в отдельный канал для побочной обработки
listen :: (Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w) -- перепаковка структуры

censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w) -- дополнительная трансформация лога

-- {-# LANGUAGE InstanceSigs #-}
-- import Control.Monad.Trans
-- class MonadTrans t where
--   lift :: Monad m => m a -> t m a

{--
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \st -> do
    a <- m
    return (a, st)

--}

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)


-- import Control.Applicative (liftA2)
-- newtype Except e a = Except { runExcept :: Either e a } -- двух-параметрический конструктор
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- довольно просто, нет стрелочных типов, есть тип-сумма Either, завернутый в абстрактную монаду

-- тривиальный конструктор, берет ийзер и заворачивает его в монаду и в иксептТ
except :: (Monad m) => Either e a -> ExceptT e m a
except = ExceptT . return

instance (Functor m) => Functor (ExceptT e m) where
  fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
  fmap f = ExceptT . fmapper . runExceptT where fmapper = fmap (fmap f)
  -- фмап для ийзер и фмап для абстрактной монады эм (которая также функтор)

{--
-- реализация не соответствует семантике иксепта, см. лекции
instance (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right -- также, просто упаковка по слоям, пюре это внутренняя абстрактная монада эм

  f <*> v = ExceptT $ liftA2 updater (runExceptT f) (runExceptT v) where -- n.b. liftA2
    updater (Left e) _ = Left e
    updater (Right g) x = fmap g x
-- почему это один-в-один повторяет Applicative Except?
-- потому, что лифт поднимает этот код в промежуточный слой этой нашей абстрактной монады,
-- получается красиво, лифт это круто.
--   f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v) -- вариант с аплайд овер для аппликатива Either
-- `(<*>) (runExceptT f) (runExceptT v)` вот это поднимается лифтом в абстрактрую монаду (здесь аппликатив)
--}
-- корректная реализация
instance (Monad m) => Applicative (ExceptT e m) where
  pure x = ExceptT $ pure (Right x)
  (ExceptT mef) <*> (ExceptT mea) = ExceptT $ do -- залезли в монаду нижнего слоя
    ef <- mef -- из монады в которой лежит `Either e f` вынимаем этот ийзер
    case ef of -- и в зависимости от значения типа-суммы:
      Left  e -> return $ Left e -- ошибка, двигаем ошибку и ничего не делаем, только пакуем ошибку в монаду `m`
      Right f -> fmap (fmap f) mea -- поднимаем функцию эф через два слоя, happy path

instance (Monad m) => Monad (ExceptT e m) where
  m >>= k = ExceptT $ do -- залезли во внутреннюю монаду, работаем с Either
    a <- runExceptT m -- левое вычисление, a :: Either, (runExceptT m) :: Monad Either
    case a of
      Left e -> return (Left e) -- запакуем обратно в монаду
      Right x -> runExceptT (k x) -- аналогично, но наоборот, распакуем в монаду (k x :: ExceptT)

--   fail = ExceptT . fail -- делегируем обработку ошибок во внутренний слой, монаду

instance MonadTrans (ExceptT e) where
  lift :: (Monad m) => m a -> ExceptT e m a
  lift = ExceptT . (fmap Right) -- поднимаем конструктор Either в монаду m

throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left -- последовательное заворачивание ошибки в слои оберток

catchE :: (Monad m) => ExceptT e m a -> (e -> ExceptT e2 m a) -> ExceptT e2 m a
m `catchE` h = ExceptT $ do -- ошибка и функция обработки ошибки (handler), уходим в монаду
  a <- runExceptT m -- вынимаем из монады значение Either
  case a of
    Left  l -> runExceptT (h l) -- вынимаем монаду с обработанной ошибкой
    Right r -> return (Right r) -- упаковываем в монаду правильный результат

testE :: (Num s) => ExceptT e (StateT s Identity) a -> (Either e a, s)
testE f = runIdentity (runStateT (runExceptT f) 3) -- s: state, e: error, a: any


type SiWs = StateT Integer (WriterT String Identity)
-- state int, writer string: стейт (инт) поверх врайтера (стринг)

test :: SiWs () -- значение: юнит (пустой тупль)
test = do
  x <- get -- взяли стейт
  when (x > 100) (lift $ tell "Overflow") -- записали в лог (через лифт)
  put 42 -- положили стейт (если нет переполнения)

runSiWs :: SiWs a -> Integer -> ((a, Integer), String)
runSiWs m = runIdentity . runWriterT . (runStateT m)


-- {-# LANGUAGE MultiParamTypeClasses #-}
infixl 7 ***
-- class Mult a b c where -- мульти-параметрический тайп-класс
--   (***) :: a -> b -> c -- сделаем его пригодным для умножения разный типов чисел

instance Mult Int Int Int where
  (***) = (*)

instance Mult Double Double Double where
  (***) = (*)

instance Mult Int Double Double where
  i *** d = fromIntegral i * d

instance Mult Double Int Double where
  d *** i = d * fromIntegral i

-- {-# LANGUAGE FunctionalDependencies #-}
class Mult a b c | a b -> c where
  (***) :: a -> b -> c
-- `| a b -> c`
-- описана зависимость, говорящая (в конечном итоге) что тип выхода определяется типами входа


{--
Предположим, что мы дополнительно реализовали строгую версию аппликативного функтора `Writer`:

newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance (Monoid w)=> Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)
  
  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

и определили такие вычисления:

actionLazy = Writer (42, "Hello!")
actionStrict = StrictWriter (42, "Hello!")

Какие из следующих вызовов приведут к расходимостям?

Select all correct options from the list
- fst . runWriter $ take 5 <$> sequenceA (repeat actionLazy)
- runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
- runWriter $ take 5 <$> sequenceA (repeat actionLazy)
- fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
--}

newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance (Monoid w)=> Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)

  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

newtype LazyWriter w a = LazyWriter { runLazyWriter :: (a, w) }

instance Functor (LazyWriter w) where
  fmap f  = LazyWriter . updater . runLazyWriter
    where updater ~(x, log) = (f x, log)

instance (Monoid w)=> Applicative (LazyWriter w) where
  pure x  = LazyWriter (x, mempty)

  f <*> v = LazyWriter $ updater (runLazyWriter f) (runLazyWriter v)
    where updater ~(g, w) ~(x, w') = (g x, w `mappend` w')

actionLazy = LazyWriter (42, "Hello!")
actionStrict = StrictWriter (42, "Hello!")

test1 = fst . runLazyWriter $ take 5 <$> sequenceA (repeat actionLazy) -- [42,42,42,42,42]
test4 = fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict) -- hang
test2 = runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict) -- hang
test3 = runLazyWriter $ take 5 <$> sequenceA (repeat actionLazy) -- inf log


{--
Сделайте на основе типа данных

data Logged a = Logged String a deriving (Eq,Show)

трансформер монад
LoggT :: (* -> *) -> * -> *
с одноименным конструктором данных и меткой поля `runLoggT`:

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

Для этого реализуйте представителя класса типов `Monad` для `LoggT m :: * -> *`
для произвольной монады `m`

instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
  fail msg = undefined

Для проверки используйте функции:

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

которые (при правильной реализации монады) должны вести себя так:

GHCi> runIdentity (runLoggT logTst)
Logged "AAABBB" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]
--}
instance (Monad m)=> Monad (LoggT m) where
  -- fail = LoggT . fail
  return = pure
  (>>=) :: LoggT m a -> (a -> LoggT m b) -> LoggT m b
  m >>= k = LoggT $ do
    ~(Logged s1 x1) <- runLoggT m
    ~(Logged s2 x2) <- runLoggT (k x1)
    return $ Logged (s1 ++ s2) x2

instance (MonadFail m)=> MonadFail (LoggT m) where
  fail :: String -> LoggT m a
  fail = LoggT . fail

instance (Applicative m)=> Applicative (LoggT m) where
  pure :: a -> LoggT m a
  pure = LoggT . pure . (Logged "") -- pure x = LoggT (pure (Logged "" x))

  (<*>) :: LoggT m (a -> b) -> LoggT m a -> LoggT m b
  f <*> v = LoggT $ liftA2 updater (runLoggT f) (runLoggT v) where
    updater ~(Logged s1 g) ~(Logged s2 x) = Logged (s1 ++ s2) (g x)

instance (Functor m)=> Functor (LoggT m) where
  fmap :: (a -> b) -> LoggT m a -> LoggT m b
  fmap f = LoggT . (fmap updater) . runLoggT where
    updater ~(Logged log x) = Logged log (f x)

{--
я тут вижу WriterT, только замаскированный. Logged это пара (a, String)

import           Control.Monad
import           Data.Monoid
instance Monad m => Functor (LoggT m) where fmap = liftM
instance Monad m => Applicative (LoggT m) where pure = return; (<*>) = ap
instance Monad m => Monad (LoggT m) where
  return = LoggT . return . Logged ""
  mx >>= f = LoggT $ do
    ~(Logged s  x) <- runLoggT mx
    ~(Logged s' y) <- runLoggT (f x)
    return $ Logged (s <> s') y
  fail = LoggT . fail

instance (Monoid w, Monad m)=> Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= k = WriterT $ do -- ду-нотация залезает во внутреннюю монаду, внешнюю оболочку снимают метки доступа `runWriterT`
        ~(v1, w1) <- runWriterT m -- левая часть, лениво
        ~(v2, w2) <- runWriterT (k v1) -- правая часть, лениво
        return (v2, w1 `mappend` w2) -- упаковали во внутреннюю монаду
    fail :: String -> WriterT w m a -- вопрос, что делать при ошибках? ХЗ, зависит от семантики
    fail = WriterT . fail -- делегируем обработку ошибки во внутреннюю монаду (если это спиисок, то будет эффект прерывания цикла)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT (pure (x, mempty)) -- вызов пюре для внутренней монады (аппликатива)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v) -- liftA2 поднимает создание новой пары во внутреннюю монаду
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)

instance (Functor m)=> Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . (fmap updater) . runWriterT
        where updater ~(x, log) = (f x, log)
--}

-- end of solution

data Logged a = Logged String a deriving (Eq, Show)
-- LoggT :: (* -> *) -> * -> *
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30 -- (30, "AAA"), x = 30
  y <- return 10                          -- (10, ""), y = 10
  z <- LoggT $ Identity $ Logged "BBB" 2  -- (2, "BBB"), z = 2
  return $ x + y + z                      -- return 42: (42, "AAA" + "" + "BBB")

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

test7 = runIdentity (runLoggT logTst) -- Logged "AAABBB" 42
test6 = runLoggT $ failTst [5,5] -- [Logged "A" 42,Logged "A" 42]
test5 = runLoggT $ failTst [5,6] -- [Logged "A" 42]
test8 = runLoggT $ failTst [7,6] -- []



{--
Напишите функцию `write2log` обеспечивающую трансформер `LoggT` стандартным логгирующим интерфейсом:

write2log :: Monad m => String -> LoggT m ()
write2log = undefined

Эта функция позволяет пользователю осуществлять запись в лог в процессе вычисления в монаде `LoggT m` 
для любой монады `m`

Введите для удобства упаковку для `LoggT Identity` и напишите функцию запускающую вычисления в этой монаде

type Logg = LoggT Identity
runLogg :: Logg a -> Logged a
runLogg = undefined

Тест

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42

должен дать такой результат:

GHCi> runLogg logTst'
Logged "AAABBB" 42

А тест (подразумевающий импорт `Control.Monad.Trans.State` и `Control.Monad.Trans.Class`)

stLog :: StateT Integer Logg Integer
stLog = do 
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100

— такой:

GHCi> runLogg $ runStateT stLog 2
Logged "30" (300,42)
--}
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class

write2log :: (Monad m)=> String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT -- runLogg (LoggT m) = runIdentity m

-- end of solution

logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100

test9 = runLogg $ runStateT stLog 2 -- Logged "30" (300,42)
test10 = runLogg logTst' -- Logged "AAABBB" 42


{--
В последнем примере предыдущей задачи функция
lift :: (MonadTrans t, Monad m) => m a -> t m a 

позволяла поднять вычисление из внутренней монады (в примере это был `Logg`) во внешний трансформер (`StateT Integer`).
Это возможно, поскольку для трансформера `StateT s` реализован представитель класса типов
MonadTrans из Control.Monad.Trans.Class

Сделайте трансформер `Logg`T представителем этого класса `MonadTrans`,
так чтобы можно было поднимать вычисления из произвольной внутренней монады в наш трансформер:

instance MonadTrans LoggT where
  lift = undefined

logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

Проверка:

GHCi> runState (runLoggT logSt) 2
(Logged "30" 300,42)
--}
instance MonadTrans LoggT where
  lift :: (Monad m)=> m a -> LoggT m a
  lift = LoggT . fmap (Logged "")
  -- lift m = LoggT $ fmap (Logged "") m
  -- lift m = LoggT $ do
  --   x <- m
  --   return (Logged "" x)

-- end of solution

-- import qualified Control.Monad.Trans.State as TS
logSt :: LoggT (TS.State Integer) Integer
logSt = do
  lift $ TS.modify (+1)
  a <- lift TS.get
  write2log $ show $ a * 10
  lift $ TS.put 42
  return $ a * 100

test11 = TS.runState (runLoggT logSt) 2 -- (Logged "30" 300,42)


{--
Реализуйте функции `evalStateT` и `execStateT`
--}

{--
-- import qualified Control.Monad.Trans.State as TS
evalStateT :: (Monad m)=> TS.StateT s m a -> s -> m a
evalStateT st = fmap fst . TS.runStateT st
-- evalStateT m s = fst <$> (TS.runStateT m s)

execStateT :: (Monad m)=> TS.StateT s m a -> s -> m s
execStateT st = fmap snd . TS.runStateT st
--}

-- end of solution

{--
Нетрудно понять, что монада `State` более «сильна», чем монада `Reader`:
вторая тоже, в некотором смысле, предоставляет доступ к глобальному состоянию,
но только, в отличие от первой, не позволяет его менять.

Покажите, как с помощью `StateT` можно эмулировать `ReaderT`:

GHCi> evalStateT (readerToStateT $ asks (+2)) 4
6
GHCi> runStateT  (readerToStateT $ asks (+2)) 4
(6,4)
--}

-- import qualified Control.Monad.Trans.Reader as TR
-- import qualified Control.Monad.Trans.State as TS
readerToStateT :: (Monad m)=> TR.ReaderT r m a -> TS.StateT r m a
readerToStateT (TR.ReaderT k) = TS.StateT (\st -> convert st) where
  convert st = (\x -> (x, st)) <$> (k st) -- (s -> m a) -> (s -> m (a, s))
-- readerToStateT rt = TS.StateT $ \s -> do
--   let ma = TR.runReaderT rt s
--   a <- ma
--   return (a, s)
{--
readerToStateT (ReaderT f) = StateT $ \s -> do { x <- f s; return (x, s) }

readerToStateT = StateT . (fmap <$> flip (,) <*>) . runReaderT -- жонглирование стрелками, охуенчик

readerToStateT rd = StateT fun where fun r = (\a -> (a, r)) <$> runReaderT rd r

readerToStateT (ReaderT rr) = StateT $ \s -> fmap (flip (,) s) (rr s)
--}
-- end of solution

test13 :: IO Integer
test13 = TS.evalStateT (readerToStateT $ TR.asks (+2)) 4 -- 6
test12 :: IO (Integer, Integer)
test12 = TS.runStateT  (readerToStateT $ TR.asks (+2)) 4 -- (6,4)


{--
Какие из перечисленных конструкторов типов являются представителями класса `Applicative`? 
(Мы пока ещё не видели реализацию представителя класса `Monad` для нашего трансформера `StateT`, 
но предполагается, что все уже догадались, что она скоро появится.)

Select all correct options from the list
- StateT Int (Writer (Sum Int))
- StateT () (StateT Int (Const ()))
- StateT () (ReaderT Int (Writer String))
- StateT Int ZipList
- StateT () (ReaderT Int ZipList)
- StateT String (Const Int)
--}
type X1 = TS.StateT Int (TW.Writer (Sum Int)) -- yes, райтер над моноидом
type X3 = TS.StateT () (TR.ReaderT Int (TW.Writer String))

type X2 = TS.StateT () (TS.StateT Int (Const ())) -- no, нет монады над Конст но она нужна стейту
type X4 = TS.StateT Int ZipList -- no, нет монады над зиплист но она нужна стейту
type X5 = TS.StateT () (TR.ReaderT Int ZipList) -- no, аналогично
type X6 = TS.StateT String (Const Int) -- no, аналогично

test14 :: X1 Char
test14 = pure 'a'
test15 :: X3 Char
test15 = pure 'b'


{--
Неудачное сопоставление с образцом аварийно прерывает вычисление
(для реализованного на предыдущих видео-степах трансформера `StateT`):

GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
GHCi> runStateT (do {6 <- sl2; return ()}) 5
*** Exception: Pattern match failure in do expression ...

Исправьте реализацию таким образом, чтобы обработка такой ситуации переадресовывалась бы внутренней монаде:

GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
GHCi> runStateT (do {6 <- sl2; return ()}) 5
[((),4)]
GHCi> sm = StateT $ \st -> Just (st+1,st-1)
GHCi> runStateT (do {42 <- sm; return ()}) 5
Nothing
--}
-- import Control.Monad.Trans.Class -- not working
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = fmap snd . runStateT m

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = fmap fst . runStateT m

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)

  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  -- fail = lift . fail where lift m = StateT (\st -> (\x -> (x, st)) <$> m)
  -- fail  = StateT . const . fail -- alternative
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

-- end of solution

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  -- lift m = StateT $ \st -> do { a <- m; return (a, st) }
  lift m = StateT (\st -> (\x -> (x, st)) <$> m)

instance (MonadFail m)=> MonadFail (StateT s m) where
  fail :: String -> StateT s m a
  fail = lift . fail where
    -- lift :: Monad m => m a -> StateT s m a
    lift m = StateT (\st -> (\x -> (x, st)) <$> m)

sl2 = StateT $ \st -> [(st, st), (st+1, st-1)]
test17 = runStateT (do {6 <- sl2; return ()}) 5 -- [((),4)]
sm = StateT $ \st -> Just (st+1,st-1)
test16 = runStateT (do {42 <- sm; return ()}) 5 -- Nothing


{--
Те из вас, кто проходил первую часть нашего курса, конечно же помнят, последнюю задачу из него.
В тот раз всё закончилось монадой `State`, но сейчас с неё все только начинается!

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

Вам дано значение типа `Tree ()`, иными словами, вам задана форма дерева.
От вас требуется сделать две вещи:
во-первых, пронумеровать вершины дерева, обойдя их `in-order` обходом (LNR: левое поддерево, вершина, правое поддерево);
во-вторых, подсчитать количество листьев в дереве.

GHCi> numberAndCount (Leaf ())
(Leaf 1,1)
GHCi> numberAndCount (Fork (Leaf ()) () (Leaf ()))
(Fork (Leaf 1) 2 (Leaf 3),2)

Конечно, можно решить две подзадачи по-отдельности, но мы сделаем это всё за один проход.

Если бы вы писали решение на императивном языке, вы бы обошли дерево,
поддерживая в одной переменной следующий доступный номер для очередной вершины,
а в другой — количество встреченных листьев, причем само значение второй переменной,
по сути, в процессе обхода не требуется. 

Значит, вполне естественным решением будет завести состояние для первой переменной, а количество листьев накапливать в «логе»-моноиде.

Вот так выглядит код, запускающий наше вычисление и извлекающий результат:

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go = undefined

Вам осталось только описать само вычисление — функцию `go`
--}
-- data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)

-- Нам надо уметь отделять листы от форков, ибо: количество листов и нумерация нод.
go :: Tree () -> TS.StateT Integer (TW.Writer (Sum Integer)) (Tree Integer)
go = traverse_ withState where
  traverse_ f (Leaf x) = let res = Leaf <$> (f x) in res <* do { lift $ TW.tell (Sum 1) }
  traverse_ f (Fork l x r) = Fork <$> (traverse_ f l) <*> (f x) <*> (traverse_ f r)
  withState _ = do          -- _ это юнит, игнорим
    n <- TS.get             -- взять текущий номер из стейта
    TS.modify succ          -- следующий номер положить в стейт
    -- lift $ TW.tell (Sum 1)  -- добавить в лог врайтера 1 (но только если это лист!)
    return n                -- номер узла

{--
go (Leaf _)     = lift (tell (Sum 1)) >> Leaf <$> pick
go (Fork l m r) = Fork <$> go l <*> pick <*> go r
pick = state ((,) <$> id <*> (+1))

go (Leaf _)            = liftM  Leaf step <* lift (tell $ Sum 1)
go (Fork left _ right) = liftM3 Fork (go left) step (go right)
step                   = get <* modify succ

go :: Tree () -> TS.StateT Integer (TW.Writer (Sum Integer)) (Tree Integer)
на входе дерево, на выходе матрешка (трансформер):
Стейт инт монада, 
где монада: врайтер (лог:сумма, значение:дерево-номер)
где сумма это количество листьев, значение ноды это порядковый номер ноды (ин-ордер)

Значит: надо сделать траверс дерева,
записывая в стейт текущий номер ноды, добавляя в "лог" врайтера 1 при встрече листа;
заменяя значение дерева с () на номер из стейта

Это можно было бы сделать обычным траверсом, но: траверс обойдет все ноды не разбирая лист/форк.
Нам надо уметь отделять листы от форков, ибо: количество листов и нумерация нод.

ghci> :t traverse
  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
--}

-- end of solution

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> TW.runWriter (TS.evalStateT (go t) 1)

test19 = numberAndCount (Leaf ()) -- (Leaf 1,1)
test18 = numberAndCount (Fork (Leaf ()) () (Leaf ())) -- (Fork (Leaf 1) 2 (Leaf 3),2)

instance Functor Tree where fmap = fmapDefault
instance Foldable Tree where foldMap = foldMapDefault
instance Traversable Tree where
  traverse g (Leaf x) = Leaf <$> (g x)
  traverse g (Fork l x r) = Fork <$> (traverse g l) <*> (g x) <*> (traverse g r)
-- foo tree = traverse (const $ TS.modify succ >> TS.get) tree


{--
Представьте, что друг принес вам игру.
В этой игре герой ходит по полю.
За один ход он может переместиться на одну клетку вверх, вниз, влево и вправо (стоять на месте нельзя).
На поле его поджидают различные опасности, такие как пропасти (chasm) и ядовитые змеи (snake).
Если игрок наступает на клетку с пропастью или со змеёй, он умирает.

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

Карта задается функцией, отображающей координаты клетки в тип этой самой клетки:

type Point = (Integer, Integer)
type GameMap = Point -> Tile

Ваша задача состоит в том, чтобы реализовать функцию
moves :: GameMap -> Int -> Point -> [Either DeathReason Point]

принимающую карту, количество шагов и начальную точку,
а возвращающую список всех возможных исходов (с повторениями),
если игрок сделает заданное число шагов из заданной точки. 

Заодно реализуйте функцию
waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int

показывающую, сколькими способами игрок может умереть данным способом, сделав заданное число шагов из заданной точки.

Например, для такого поля:

поле в виде рисунка:
 | 0 1 2 3 4 5
--------------
0| o o o o o o
1| o       s o
2| o   s     o
3| o         o
4| o         o
5| o o o o o o
закодировано как:

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

ожидаются такие ответы:

GHCi> waysToDie Poisoned map1 1 (4,2)
1  -- можно пойти к змее наверх
GHCi> waysToDie Poisoned map1 2 (4,2)
2  -- можно пойти к змее наверх или к змее влево
GHCi> waysToDie Poisoned map1 3 (4,2)
5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
   -- а к правой — уже четырьмя (вверх, влево-вверх-вправо,
   --                            влево-вправо-вверх, вниз-вверх-вверх)
GHCi> waysToDie Poisoned map1 4 (4,2)
13

Гарантируется, что изначально игрок стоит на пустой клетке.

Подсказка: не забывайте, в каком уроке эта задача.
--}

{--
-- сколько путей умереть данным способом, сделав заданное число шагов из заданной точки.
waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie death gmap steps startP = length selected where
  allDestinations = (moves gmap steps startP)
  selected = filter (chosen death) allDestinations -- `debug` ("all: " ++ show allDestinations)
  chosen death d = case d of
    Right _ -> False
    Left reason -> reason == death

-- принимающую карту, количество шагов и начальную точку,
-- а возвращающую список всех возможных исходов (с повторениями),
-- если игрок сделает заданное число шагов из заданной точки
moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gmap steps point = moves_ gmap steps [Right point]

moves_ :: GameMap -> Int -> [Either DeathReason Point] -> [Either DeathReason Point]
moves_ _ _ [] = []
moves_ _ 0 eps = eps -- eps: list of Ether (err) Point
moves_ gmap steps (ep:eps) = oneStep ++ rest where
  rest = moves_ gmap steps eps
  oneStep = case ep of
    Left _ -> [ep]
    Right p -> moves_ gmap (steps-1) next4 where
      next4 = (gamePoint gmap) <$> (fourWays p)

fourWays (x, y) = [p1, p2, p3, p4] where
  p1 = (x+1,  y)
  p2 = (x-1,  y)
  p3 = (x,    y+1)
  p4 = (x,    y-1)

gamePoint :: GameMap -> Point -> Either DeathReason Point
gamePoint g p = case g p of
  Floor -> Right p
  Chasm -> Left Fallen
  Snake -> Left Poisoned

Есть кол-во шагов,
На каждом шаге текущая точка меняет х или у на 1 или -1, т.е. 4 варианта новой точки на каждом шаге,
Умерев на точке пэ, дальше из этой точки не двигаемся,
Нужно получить список всех конечных состояний после эн шагов.
Значит, промежуточные удачные точки мы не записываем, только держим дерево конечных состояний.

> move должна возвращать только исходы, то есть на какой клетке вы остановитесь после последнего хода
ключевое слово "исходов"
--}

-- throwE' = ExceptT . return . Left

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n = runExceptT . (moves' m n)
-- вычисление трансформера иксептТ (список изеров), простая рекурсия
moves' :: GameMap -> Int -> Point -> ExceptT DeathReason [] Point
moves' m n p = case m p of -- тре кейса, три конструктора иксептТ
    Chasm -> throwE Fallen
    Snake -> throwE Poisoned
    Floor -> if n == 0 then return p
      else ExceptT ( -- конструктор желаемого трансформера, внутри рекурсивный вызов
        moves m (n - 1) `concatMap` nexts p
      ) -- маппит функцию point -> [either] на список 4 точек

nexts :: Point -> [Point]
nexts (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie d m n = length . filter (== Left d) . moves m n

-- end of solution

data Tile = Floor | Chasm | Snake deriving Show
data DeathReason = Fallen | Poisoned deriving (Eq, Show)
-- Карта задается функцией, отображающей координаты клетки в тип этой самой клетки:
type Point = (Integer, Integer)
type GameMap = Point -> Tile

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

test23 = waysToDie Poisoned map1 1 (4,2) -- 1  -- можно пойти к змее наверх
test22 = waysToDie Poisoned map1 2 (4,2) -- 2  -- можно пойти к змее наверх или к змее влево
test21 = waysToDie Poisoned map1 3 (4,2) -- 5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
   -- а к правой — уже четырьмя (вверх, влево-вверх-вправо,
   --                            влево-вправо-вверх, вниз-вверх-вверх)
test20 = waysToDie Poisoned map1 4 (4,2) -- 13

{--
wrong, возвращает промежуточные точки маршрутов:
moves gmap steps (x, y) = list where
  list = (moves_ gmap steps p1) 
      ++ (moves_ gmap steps p2) 
      ++ (moves_ gmap steps p3) 
      ++ (moves_ gmap steps p4)
  p1 = (x+1, y)
  p2 = (x-1, y)
  p3 = (x, y+1)
  p4 = (x, y-1)

moves_ :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves_ _ 0 _ = []
moves_ gmap steps p = case gamePoint gmap p of
  Left d -> [Left d] -- stop here
  Right _ -> (Right p) : ( (moves_ gmap (steps-1) p1)
                        ++ (moves_ gmap (steps-1) p2)
                        ++ (moves_ gmap (steps-1) p3)
                        ++ (moves_ gmap (steps-1) p4)
    ) where
      (x, y) = p
      p1 = (x+1, y)
      p2 = (x-1, y)
      p3 = (x, y+1)
      p4 = (x, y-1)

--}


{--
Следующий код

import Control.Monad.Trans.Maybe
import Data.Char (isNumber, isPunctuation)

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

используя трансформер `MaybeT` и свойства функции `msum`,
отвергает ввод пользовательского пароля, до тех пор пока он не станет удовлетворять заданным критериям.

Это можно проверить, вызывая его в интерпретаторе
GHCi> runMaybeT askPassword0

Используя пользовательский тип ошибки и трансформер `ExceptT` вместо `MaybeT`,
модифицируйте приведенный выше код так, чтобы он
выдавал пользователю сообщение о причине, по которой пароль отвергнут.

data PwdError = PwdError String
type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorIOMonad String
getValidPassword = undefined

Ожидаемое поведение:

GHCi> runExceptT askPassword
Enter your new password:
qwerty
Incorrect input: password is too short!
qwertyuiop
Incorrect input: password must contain some digits!
qwertyuiop123
Incorrect input: password must contain some punctuation!
qwertyuiop123!!!
Storing in database...
GHCi>
--}

-- import Control.Monad.Trans.Except
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Foldable (msum)
-- import Data.Char (isNumber, isPunctuation)

getValidPassword :: PwdErrorIOMonad String -- TE.ExceptT PwdError IO String -- PwdError = String
-- т.е. имеем ио монаду в которой изер из ошибка=строка, значение=строка
getValidPassword = do
  s <- liftIO getLine
  let errOrpass = check s
  when (isLeft errOrpass) (runError errOrpass)
  return s

-- runError :: (MonadIO m)=> Either PwdError a -> TE.ExceptT PwdError m b
runError (Right _) = undefined
runError (Left (PwdError msg)) = do
  liftIO (putStrLn $ "Incorrect input: " ++ msg)
  TE.throwE (PwdError msg)

-- check :: String -> Either PwdError String
check s
  | length s < 8              = Left $ PwdError "password is too short!"
  | not (any isNumber s)      = Left $ PwdError "password must contain some digits!"
  | not (any isPunctuation s) = Left $ PwdError "password must contain some punctuation!"
  | otherwise                 = Right s

instance Semigroup PwdError where (PwdError x) <> (PwdError y) = PwdError $ x <> y
instance Monoid PwdError where mempty = PwdError mempty

{--
class Semigroup a where
  (<>) :: a -> a -> a
--}

-- end of solution

newtype PwdError = PwdError String
type PwdErrorIOMonad = TE.ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

-- test25 = runExceptT askPassword
{--
Enter your new password:
qwerty
Incorrect input: password is too short!
qwertyuiop
Incorrect input: password must contain some digits!
qwertyuiop123
Incorrect input: password must contain some punctuation!
qwertyuiop123!!!
Storing in database... 
--}


{--
Вспомним функцию `tryRead`:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a

Измените её так, чтобы она работала в трансформере `ExceptT`
--}
-- import Text.Read ( readMaybe, readEither, )
tryRead :: (Read a, Monad m)=> String -> TE.ExceptT ReadError m a
tryRead "" = TE.throwE EmptyInput
tryRead s = parse `TE.catchE` errHandler where
  parse = TE.except (readEither s)
  errHandler _ = TE.throwE (NoParse s)

{--
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)

--}

-- end of solution

data ReadError = EmptyInput | NoParse String deriving Show


{--
С деревом мы недавно встречались:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

Вам на вход дано дерево, содержащее целые числа, записанные в виде строк.
Ваша задача обойти дерево `in-order` (левое поддерево, вершина, правое поддерево) и
просуммировать числа до первой строки, 
которую не удаётся разобрать функцией `tryRead` из прошлого задания (или до конца дерева, если ошибок нет). 
Если ошибка произошла, её тоже надо вернуть.

Обходить деревья мы уже умеем, так что от вас требуется только функция `go`, подходящая для такого вызова:

treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
(Just (NoParse "oops"),3)
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
(Nothing,34)
--}
-- data Tree a = Leaf a | Fork (Tree a) a (Tree a)
-- data ReadError = EmptyInput | NoParse String deriving Show
go2 :: String -> TE.ExceptT ReadError (TW.Writer (Sum Integer)) () -- (sum-int, either-err/unit)
go2 s = do
  n <- tryRead s -- :: TE.ExceptT ReadError (TW.Writer (Sum Integer)) Integer
  lift $ TW.tell (Sum n)

{--
tryRead :: (Read a, Monad m)=> String -> TE.ExceptT ReadError m a
tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)
--}

-- end of solution

treeSum t = let (err, sum) = TW.runWriter . TE.runExceptT $ traverse_ go2 t
            in (maybeErr err, getSum sum)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

test25 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16") -- (Just (NoParse "oops"),3)
test24 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16") -- (Nothing,34)

{--
Предположим мы хотим реализовать следующую облегченную версию функтора,
используя многопараметрические классы типов:

class Functor' c e where
  fmap' :: (e -> e) -> c -> c

Добавьте в определение этого класса типов необходимые функциональные зависимости
и реализуйте его представителей для списка и `Maybe` так,
чтобы обеспечить работоспособность следующих вызовов

GHCi> fmap' succ "ABC"
"BCD"
GHCi> fmap' (^2) (Just 42)
Just 1764
--}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
class Functor' c e | c -> e where fmap' :: (e -> e) -> c -> c
instance (Eq a)=> Functor' [a] a where fmap' = fmap
instance (Num a, Eq a)=> Functor' (Maybe a) a where fmap' = fmap

-- end of solution

-- test27 :: String
test27 = fmap' succ "ABC" -- "BCD"

-- test26 :: Maybe Integer
test26 = fmap' (^2) (Just 42) -- Just 1764
test28 = take 5 (fmap' (^ 2) [2, 3 .. ])
