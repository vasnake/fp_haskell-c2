{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
{-# LANGUAGE FunctionalDependencies #-}
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

module TestTransformers where

import Text.Parsec ( getParserState )
import Data.Char ( toLower, toUpper )
import Data.Function ( (&) )

import Data.Monoid (
    Sum(..), Product(..), Endo(..), appEndo, (<>), Dual(..), First(..)
    )

import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor ( (<&>) )

import Control.Applicative (
    Alternative(..), Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>), liftA2,
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when, MonadFail )
-- import Control.Monad.Cont ( callCC )
-- import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
-- import Control.Applicative (Alternative(..))

import Control.Monad.Trans.Class
-- import Control.Monad.Trans ( lift )
import qualified Control.Monad.Trans.Writer as TW
import qualified Control.Monad.Trans.Reader as TR
import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.Trans.State as TS
-- import Control.Monad.Trans.Writer ( runWriter, tell, Writer )
-- import Control.Monad.Trans.Reader as TR hiding (reader, Reader, ReaderT, runReader, runReaderT )
-- import Control.Monad.Trans.Reader as TR ( asks) -- ( ask, asks, Reader(..), ReaderT(..) )

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, pred, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take, Monad(..),
    Double, either, Integer, head, tail, IO(..), snd, pi, fromIntegral,
    repeat, fst, snd,
    )

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
