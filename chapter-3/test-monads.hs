-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

{-# HLINT ignore "Use traverse_" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Using foldr on tuple" #-}
{-# HLINT ignore "Using maximum on tuple" #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use let" #-}

module TestMonads where

import Text.Parsec (getParserState)
import Data.Char (toLower)
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

import Control.Monad ( liftM, mplus, guard, mfilter, ap, guard, MonadPlus(..), when )
-- import Control.Monad.Cont ( callCC )
-- import Control.Monad (liftM, ap, MonadPlus(..), guard, msum)
-- import Control.Applicative (Alternative(..))

-- import GHC.Show (Show)
-- import GHC.Base (Eq)
import Prelude (
    show, read, String, Char, Functor(..), fmap, Bool, otherwise, Int,
    (==), (*), id, const, Maybe(..), null, ($), succ, (.), undefined, Num(..), Show, Eq,
    foldr, foldl, Either(..), Monoid(..), Semigroup(..), putStrLn, print, (*), (>), (/), (^),
    map, (=<<), (>>=), return, flip, (++), fail, Ord(..), (>>), take, Monad(..),
    Double, either, Integer
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
