-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

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
    Alternative(..), Applicative(..), (<*>), (<$>), ZipList(..), (<**>), (<|>)
    )

import Data.Foldable (
    Foldable(..), fold, foldMap, maximum, sequenceA_, sequence_, traverse_, msum
    )

import Data.Traversable (
    sequence, sequenceA, Traversable(..), traverse, fmapDefault, foldMapDefault, mapM
    )

-- import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when )
-- import Control.Monad.Cont ( callCC )
-- import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
-- import Control.Applicative (Alternative(..))

-- import Control.Monad.Trans.Reader as TR hiding (reader, Reader, ReaderT, runReader, runReaderT )
-- import Control.Monad.Trans.Reader as TR ( asks)
 -- ( ask, asks, Reader(..), ReaderT(..) )

-- import Control.Monad.Trans.Writer ( runWriter, tell, Writer )
-- import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Class

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, pred, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take, Monad(..),
    Double, either, Integer, head, tail, IO(..), snd
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

test f = runIdentity (runStateT (runExceptT f) 3)
