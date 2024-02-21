# FP Haskell, chapter 4, трансформеры монад

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-4/test-transformers.hs)

Мотивация: ???

definitions: ???

## chapter 4.1, Трансформер WriterT

https://stepik.org/lesson/38578/step/1?unit=20503

- Тип WriterT
- Функтор WriterT
- Аппликативный функтор WriterT
- Монада WriterT
- Лифтинг в WriterT
- Стандартный интерфейс для WriterT
- Совместное использование ReaderT и WriterT

### 4.1.2 writer, runWriterT, execWriterT

Writer: запись в "лог", пара, значение и моноид "лога".
Моноид: нейтральный элемент и ассоциативная бинарная операция.

Реализуем трансформер `WriterT`
```hs
import Control.Applicative (liftA2)
import Data.Tuple (swap)

newtype Writer w a = Writer { runWriter :: (a, w) } -- было
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) } -- будем реализовывать
-- заворачивание всей пары в монаду (а могли бы только первый параметр завернуть) упрощает логику доступа к содержимому,
-- каждый слой контекста снимается одной меткой доступа (run...)

-- а вот для ридера, внутренняя монада заворачивала не всю стрелку а только результат
-- `r -> m a`, хотя, казалось бы, надо было `m (r -> a)`
-- дело в том, что `r` это внешний параметр ридера, он приходит снаружи и не требует обвязки контекстом

-- example
runWriterT (WriterT (Just (42, "foo"))) -- внутренняя монада Maybe

-- конструктор
writer :: (a, w) -> Writer w a -- конструктор монады врайтер, бесполезный
writer = Writer

writer :: (Monad m) => (a, w) -> WriterT w m a
writer = WriterT . return -- упаковали во внутреннюю монаду (полиморфизм), упаковали во врайтер

runWriterT (writer (42, "foo")) :: [(Int, String)] -- сняли полиморфизм, явно указав тип внутренней монады

-- доступ к логу, если значение надо проигнорировать
execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: (Monad m) => WriterT w m a -> m w
execWriterT = (fmap snd) . runWriterT -- монада лога из монады пары получается через функтор (fmap)
-- зачем сделали требование к контексту: Monad?
-- liftM вместо fmap, в разных версиях библиотеки по разному
-- ибо есть версии, где монада не расширяет функтор

```
repl

### 4.1.3 Functor WriterT

Реализуем функтор для трансформера `WriterT`
```hs
instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f = Writer . updater . runWriter -- развернуть из оболочки, сгенерить новую пару, завернуть обратно в оболочку
        where updater ~(x, log) = (f x, log) -- ленивый пат.мат. `~`, irrefutable pattern
-- пара это функтор, но с обратным порядком элементов,
-- поэтому мы не можем здесь просто прокинуть fmap в пару, надо ручками реализовать

-- в стандартной либе есть обе версии функтора: ленивый и энергичный,
-- ленивый может падать по памяти из-за накопления thunk-ов, энергичный может падать на undefined или бесконечности

instance (Functor m) => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . (fmap updater) . runWriterT -- так же, только апдейт протаскивается во внутренний функтор через свой fmap
        where updater ~(x, log) = (f x, log)

-- example

runWriter (fmap (^2) $ Writer (3, "a"))
(9, "a")
runWriterT (fmap (^2) $ WriterT [(3, "a"), (4, "b")]) -- внутренняя монада: список
[(9, "a"), (16, "b")]

```
repl

### 4.1.4 Applicative WriterT

Реализуем аппликативный функтор для `WriterT`.
Теперь на лог надо наложить ограничения моноида, ибо в аппликативе надо складывать два лога.
```hs
instance (Monoid w) => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    f <*> v = Writer $ updater (runWriter f) (runWriter v)
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)
-- распаковать, сгенерить новую пару, запаковать

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT (pure (x, mempty)) -- вызов пюре для внутренней монады (аппликатива)

    -- liftA2 поднимает создание новой пары во внутреннюю монаду (аппликатив)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v)
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)

-- example
runWriter (Writer ((*6), "foo ") <*> Writer (7, "bar"))
(42, "foo bar")

runWriterT (writer ((*6), "foo ") <*> writer (7, "bar")) :: [(Int, String)] -- уточнение типа для полиморфного `writer`
[(42, "foo bar")] -- внутренняя монада (аппликатив) это список

-- покажем оба эффекта сразу (и лог и список)
runWriterT (WriterT [((^2), "^2"), ((^3), "^3")] <*> WriterT [(2, " 2"), (3, " 3")])
[(4, "^2 2"), (9, "^2 3"), (8, "^3 2"), (27, "^3 3")] -- вложенный цикл, сложенные логи

```
repl

```hs
https://stepik.org/lesson/38578/step/5?unit=20503
TODO
{--
Предположим, что мы дополнительно реализовали строгую версию аппликативного функтора `Writer`:

newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance Monoid w => Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)
  
  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

и определили такие вычисления:

actionLazy = Writer (42,"Hello!")
actionStrict = StrictWriter (42,"Hello!")

Какие из следующих вызовов приведут к расходимостям?

Select all correct options from the list
- fst . runWriter $ take 5 <$> sequenceA (repeat actionLazy)
- runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
- runWriter $ take 5 <$> sequenceA (repeat actionLazy)
- fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
--}

-- solution
-- «приведут к расходимостям при вычислении до нормальной формы»

```
test

### 4.1.6 Monad WriterT

Реализуем монаду для `WriterT`
```hs
instance (Monoid w) => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    -- реализация так записано, чтобы было похоже на ду-нотацию (см ниже, WriterT)
    m >>= k = Writer $ let -- завернули в оболочку врайтера
        (v1, w1) = runWriter m -- распаковали левую часть бинда
        (v2, w2) = runWriter (k v1) -- вычислили стрелку Клейсли (зависимость от левой часли бинда), правая часть бинда
        in (v2, w1 `mappend` w2) -- создали новую пару

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= k = WriterT $ do -- ду-нотация залезает во внутреннюю монаду, внешнюю оболочку снимают метки доступа `runWriterT`
        ~(v1, w1) <- runWriterT m -- левая часть, лениво
        ~(v2, w2) <- runWriterT (k v1) -- правая часть, лениво
        return (v2, w1 `mappend` w2) -- упаковали во внутреннюю монаду

    fail :: String -> WriterT w m a -- вопрос, что делать при ошибках? ХЗ, зависит от семантики
    fail = WriterT . fail -- делегируем обработку ошибки во внутреннюю монаду (если это спиисок, то будет эффект прерывания цикла)

-- example

runWriter $ do { x <- Writer (41, "foo"); return (succ x) }
(42, "foo")
runWriterT $ do {x <- writer (41, "foo"); return (succ x)} :: [(Int, String)] -- подсказка для полиморфного `writer`
(42, "foo")

runWriterT $ do { 555 <- writer (41, "foo"); return 42 } :: [(Int, String)]
[] -- был вызван и обработан `fail` на сломанном пат.мат. `555 <- ...`
-- очевидно, фейл обработан в монаде списка

runWriterT $ do { 1 <- WriterT [(41, "foo"), (1, "bar")]; return 42 } :: [(Int, String)]
[(42, "bar")] -- видно, что только первое вычисление в списке сломалось на плохом пат.мат.

```
repl

Разные порядки вложенности монад в трансформерах
```hs
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

a, b :: MaybeT (Writer String) ()
a = do
  lift $ tell "abc"
  fail ""

b = do
  lift $ tell "def"
  pure ()

c, d :: WriterT String Maybe ()
c = do
  tell "abc"
  fail ""

d = do
  tell "def"
  pure ()

ghci> a >> b
MaybeT (WriterT (Identity (Nothing,"abc")))
ghci> b >> a
MaybeT (WriterT (Identity (Nothing,"defabc")))
ghci> c >> d
WriterT Nothing
ghci> d >> c
WriterT Nothing

```
extra

memoization, Pascal triangle
```hs
import Data.List (intercalate)

pascal :: [[Integer]]
pascal = repeat 1 : map f pascal
  where f xs = let ys = 1 : zipWith (+) (tail xs) ys in ys

binomial :: Int -> Int -> Integer
binomial n k = pascal !! (n - k) !! k

binomial' :: Int -> Int -> Integer
binomial' n k = foldl1 seq $ concatMap (take (k + 1)) (take (n - k + 1) pascal)

main = do
  mapM_ (putStrLn . intercalate "\t" . map show) $ take 10 (map (take 10) pascal)
  -- print (binomial 5000 2000) -- медленно
  print (binomial' 5000 2000) -- быстро

--- another attempt

import Data.List (intercalate)

pascal :: [[Integer]]
pascal = [[f n k | k <- [0..]] | n <- [0..]]
  where
    f :: Int -> Int -> Integer
    f 0 _ = 1
    f _ 0 = 1
    f n k = pascal !! (n - 1) !! k + pascal !! n !! (k - 1)

binomial' :: Int -> Int -> Integer
binomial' n k = foldl1 seq $ concatMap (take (k + 1)) (take (n - k + 1) pascal)

main = do
  mapM_ (putStrLn . intercalate "\t" . map show) $ take 10 (map (take 10) pascal)
  print (binomial' 1000 500) -- не так быстро, но все еще неплохо
```
extra

```hs
https://stepik.org/lesson/38578/step/7?unit=20503
TODO
{--
Сделайте на основе типа данных

data Logged a = Logged String a deriving (Eq,Show)

трансформер монад
LoggT :: (* -> *) -> * -> *
с одноименным конструктором данных и меткой поля `runLoggT`:

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

Для этого реализуйте для произвольной монады `m` представителя класса типов `Monad` для `LoggT m :: * -> *`

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

которые при правильной реализации монады должны вести себя так:

GHCi> runIdentity (runLoggT logTst)
Logged "AAABBB" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]
--}
instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
  fail msg = undefined

-- solution

```
test

### 4.1.8 MonadTrans lift

Дополнительно надо реализовать `lift` для подъема мроизвольной монады в трансформер
```hs
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

instance (Monoid w) => MonadTrans (WriterT w) where
    lift :: (Monad m) => m a -> WriterT w m a
    lift m = WriterT $ do
        x <- m -- вынули из пришедшей монады
        return (x, mempty) -- засунули во внутреннюю монаду, добавив пустой лог

-- example
wl3 = WriterT $ [(1, "one"), (10, "ten"), (20, "twenty")]
runWriterT $ do { x <- wl3; f <- lift [pred, succ]; return (f x) }
[(0, "one"), (2, "one"), (9, "ten"), (11, "ten"), (19, "twenty"), (21, "twenty")]
-- для каждого значения из wl3, выполнить каждую ф. из f

```
repl

### 4.1.9 tell, listen, censor

Стандартный интерфейс, `tell, listen, censor`
```hs
tell :: w -> Writer w ()
tell w = Writer ((), w)

tell :: (Monad m) => w -> WriterT w m ()
tell w = writer ((), w)

-- example
runWriterT $ do { x <- wl3; f <- lift [pred, succ]; tell " foo"; return (f x) }
[(0, "one foo"), (2, "one foo"), (9, "ten foo"), (11, "ten foo"), (19, "twenty foo"), (21, "twenty foo")]

-- по аналогии с юниксовым `tee`, выдать лог в отдельный канал для побочной обработки
listen :: (Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w) -- перепаковка структуры

censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w) -- дополнительная трансформация лога

-- example
runWriterT (censor (\(c:cs) -> [c]) wl3)
[(1, "o"),(10, "t"),(20, "t")]
```
repl

```hs
https://stepik.org/lesson/38578/step/10?unit=20503
TODO
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
write2log :: Monad m => String -> LoggT m ()
write2log s = undefined

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = undefined

-- solution

```
test

### 4.1.11 пример композиции трансформеров

Небольшое упражнение на объединение `ReaderT`, `WriterT`.
Предполагается, что автор разнес реализацию по трем модулям
```hs
import MonadTrans
import ReaderT
import WriterT

import Control.Monad.Identity
import Data.Char (toUpper)

-- это ридер
logFirstAndRetSecond :: 
    ReaderT [String] -- внешняя монада, трансформер, енв как список строк
    (WriterT String Identity) -- внутренняя монада (составлена из трансформера над айдентити), лог как строка
    String -- содержимое ридера
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift $ tell el1 -- голову записали в лог (внутренняя монада)
    return el2

strings = ["ab", "cd", "fg"]
runIdentity $ runWriterT (runReaderT logFirstAndRetSecond strings)
("CD", "ab") -- первый элемент пары: значение из ридера, второй элемент пары: строка лога

```
repl

```hs
https://stepik.org/lesson/38578/step/12?unit=20503
TODO
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
  lift = undefined

-- solution

```
test

## chapter 4.2, Трансформер StateT

https://stepik.org/lesson/38579/step/1?unit=20504

- Код из лекций этого урока 
- Тип StateT
- Функтор StateT
- Аппликативный функтор StateT
- Аппликативный функтор StateT (продолжение)
- Монада StateT
- Лифтинг в StateT
- Стандартный интерфейс для StateT

### 4.2.2 lectures code

Код из лекций
```hs
{-# LANGUAGE InstanceSigs #-}
class MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \ s -> do
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
```
repl

### 4.2.3 newtype StateT, state

На базе (ранее рассмотренной) монады `State`, реализуем монаду-трансформер `StateT`.
Это стрелочный тип, поэтому будут лямбды.
```hs
newtype State s a = State { runState :: s -> (a, s) }
-- было: стрелка из старого стейта в пару: значение, новый стейт

-- стало: аналогичная стрелка, но пара завернута во внутреннюю (абстрактную) монаду
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- обобщенный конструктор, на входе стрелка без намека на монаду,
-- монада должна быть определена требованием к возвращаемому типу
state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f) -- `return . f` завернем результат стрелки в монаду (внутреннюю)
-- ретурн: "тривиальный" конструктор, никаких сложных выражений здесь нельзя

-- example

-- тривиальный конструктор
ghci> :t runStateT (state (\st -> ("foo", st*2))) 5 -- начальный стейт = 5
  :: (Monad m, Num s) => m (String, s) -- тип полиморфный по внутренней монаде
-- подскажем тип и посмотрим на результат вычисления:
ghci> runStateT (state (\st -> ("foo", st*2))) 5 :: [(String, Int)]
[("foo",10)]

-- "родной" нетривиальный конструктор, выражения могут быть любой сложности
sm = StateT $ \st -> Just (show $ st+3, st*2)
ghci> :t sm
sm :: (Show a, Num a) => StateT a Maybe String -- внутренняя монада мейби, тип значения: строка

ghci> runStateT sm 5 -- начальный стейт = 5
Just ("8",10)

-- чуть сложнее, внутри монада списка
sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)] -- поскольку это стрелочный тип,
-- внутрь мы можем засунуть что угодно, лишь бы совпадало по типа `s -> m (a, s)`
ghci> runStateT sl3 5
[(6,42),(7,5),(8,10)]
ghci> :t sl3
sl3 :: Num a => StateT a [] a -- внутренняя монада: список, значение: тот же тип что и стейт
```
repl

```hs
https://stepik.org/lesson/38579/step/4?unit=20504
TODO
{--
Реализуйте функции `evalStateT` и `execStateT`
--}
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = undefined

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/38579/step/5?unit=20504
TODO
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
readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT = undefined

-- solution

```
test

### 4.2.6 Functor StateT

Реализуем функтор для трансформера `StateT`
```hs
-- для простого стейт было так:
instance Fuctor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap ab sa = State $ \st -> -- стрелочный тип, поэтому лямбда, начальный стейт передается снаружи
    updater (runState sa st) where updater ~(x, s) = (ab x, s) -- ленивый пат.мат

-- для трансформера с внутренней абстрактной монадой (функтором) стало так:
instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater (runStateT m st) -- ключевой момент: использование внутреннего 
  -- fmap для затаскивания функции во внутреннюю монаду
    where updater ~(x, s) = (f x, s)

ghci> :t runStateT
runStateT :: StateT s m a -> s -> m (a, s) -- функция двух аргументов,
-- берет трансформер, начальный стейт, возвращает пару в монаде

-- example

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)]
ghci> runStateT sl3 5 -- в объекте sl3 стрелка, скормив ей начальный стейт 5 получаем:
[(6,42),(7,5),(8,10)]

ghci> runStateT (fmap (^2) sl3) 5 -- начальный стейт = 5, функторим возведенеи в квадрат
[(36,42),(49,5),(64,10)] -- (5+1)^2, (5+2)^2, (5+3)^2

-- заметим, функтор может менять значения, но не может менять стейт или структуру
```
repl

### 4.2.7 Applicative State

Реализуем аппликативный функтор для трансформера `StateT`.
Аппликатив уже может (и делает) работать со стейтом.
Кстати, тут стейт прокидывается (и модифицируется) по цепочке (в отличие от ридера).
Не может работать со структурой; влияние левого вычисления на правое и структуру результата: забота монады.
```hs
-- для простого аппликатива стейт было так:
instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\s -> (x, s))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  sab <*> sa = State sb where sb = \s -> -- стейт это стрелочный тип, поэтому начальный стейт приходит снаружи, поэтому лямбда
    let (g, s1) = runState sab s -- левое вычисление на начальном стейте
        (x, s2) = runState sa s1 -- правое вычисление, стейт уже модифицирован левым вычислением
    in  (g x, s2) -- применение функции из левого к аргументу из правого
```
repl

### 4.2.8 Applicative StateT

Продолжение [предыдущей лекции](#427-applicative-state)
```hs
-- для трансформера стейт-с-внутренней-монадой стало так:
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s) -- добавился внутремонадный `return` для заворачивания пары

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do -- появлась ду-нотация для доступа во внутреннюю монаду
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')
-- обратите внимание: заменить контекст (Monad m) на (Applicative m) не получается,
-- интерфейс аппликатива не дает возможности разобрать пару, находящуюся под барьером аппликатива.

-- example

stT1 = state $ \x -> ((^2), x+2) -- функция (левый параметр аппликатива)
stT2 = state $ \x -> (x, x+3) -- значения (правый параметр аппликатива)
-- в паре первый элемент это значение, второй это стейт
runStateT (stT1 <*> stT2) 5 :: Maybe (Int, Int) -- stT1 applied over stT2, с подсказкой типа внутренней монады конструктору
Just (49,10) -- не сразу доходит, откуда 49. Разберем по шагам:
-- ((^2), 5+2) -- в левом вычислении получили пару
-- (7^2, 7+3) -- в правом вычислении получили пару

```
repl

```hs
https://stepik.org/lesson/38579/step/9?unit=20504
TODO
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

-- solution
{--
> на примере одного из случаев:
StateT () (ReaderT Int (Writer String)) - как следует из последних минут предыдущего видео, 
чтобы этот конструктор был аппликативом, ReaderT Int (Writer String) должен быть монадой.
Чтобы ReaderT Int (Writer String) был монадой, нужно, чтобы Writer String был монадой (смотри лекцию 4.1 Трансформер WriterT)
чтобы Writer String был монадой, нужно, чтобы String был моноидом

> StateT имеет несколько параметров и объявлен представителем Applicative (и Monad) не безусловно, 
а при некоторых дополнительных условиях на некоторые из этих параметров
--}

```
test

### 4.2.10 Monad StateT

Реализуем монаду для трансформера `StateT`
```hs
-- для простой монады стейт было так:
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= k = State S \s -> -- стрелочный тип внутри стейт, поэтому лямбда, начальный стейт приходит снаружи
    let (x, s2) = runState m s -- левое вычисление
    in  runState (k x) s2 -- правое вычисление, с учетом результатов левого (и структура и стейт могут меняться)

-- для трансформера стало так:
instance (Monad m) => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do -- ду-нотация для прохода во внутреннюю монаду
    ~(x, s') <- runStateT m s -- ленивый пат.мат., левое вычисление
    runStateT (k x) s' -- правое вычисление

-- example
stT1 = state $ \x -> ((^2), x+2)
stT2 = state $ \x -> (x, x+3)
ghci> runStateT (do { g <- stT1; x <- stT2; return (g x) }) 5 -- внутренняя монада IO (repl)
(49,10) -- ^2, 5+2; 7^2, 7+3
-- обратите внимание, в выражении работы с монадами стейт,
-- мы работаем со значением, стейт остается за кадром, внутри определения монад stT1, stT2

```
repl

```hs
https://stepik.org/lesson/38579/step/11?unit=20504
TODO
{--
Неудачное сопоставление с образцом для реализованного на предыдущих видео-степах 
трансформера `StateT` аварийно прерывает вычисление:

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
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

-- solution

```
test

### 4.2.12 MonadTrans StateT lift

Реализуем лифт вычислений во внутренней монаде во внешний `StateT`
```hs
instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift m = StateT $ \st -> do -- залезаем во внутреннюю монаду (лямбда отражает структуру трансформера - стрелка из внешненго стейта)
    a <- m -- вынимаем значение
    return (a, st) -- перепаковываем в нужную нам пару

-- example: сочетание эффектов стейта и списка

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)]

ghci> runStateT (do { x <- sl3; f <- lift [pred, succ]; return (x, f x)}) 5
-- стейт меняется только в первом вычислении; лифт и ретурн не могут менять стейт по определению.
[ -- sl3 дает внешний цикл, туда попадает стейт 5; лифт дает внутренний цикл, две функции обработки значений
  ((6,5),42), -- 5+1 42 pred
  ((6,7),42), -- 5+1 42 succ
  ((7,6),5), -- 5+2 st pred
  ((7,8),5), -- 5+2 st succ
  ((8,7),10), -- 5+3 st*2 pred
  ((8,9),10)] -- 5+3 st*2 succ
```
repl

### 4.2.13 StateT get, put, modify

Реализуем стандартный интерфейс: `get, put, modify`
```hs
-- запаковка переданного снаружи стейта в пару и полиморфную монаду (через конструктор `state`)
get :: (Monad m) => StateT s m s
get = state $ \s -> (s, s) -- вернуть внешний стейт

-- аналогично `get` но внешний стейт игнорится а явно переданный упаковывается
put :: (Monad m) => s -> StateT s m ()
put s = state $ \_ -> ((), s) -- положить данный стейт

-- аналогично, но теперь есть функция преобразования стейта,
-- преобразованный внешний стейт запаковывается
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s) -- преобразовать и положить внешний стейт

-- Никакого взаимодействия в внутренней монадой, работаем только со стейтом;

-- example

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)] -- внутренняя монада списка
ghci> runStateT (do {sl3; y <- get; put (y - 2); return (2 * y) }) 5
[(84,40), -- 2*42, 42-2
(10,3),   -- 2*5, 5-2
(20,8)]   -- 2*10, (5*2)-2
-- ретурн кладет в первое значение пары `2 * y`
-- где уай это стейт из предыдущего шага пайплайна (get дает нам стейт)
-- в итоге вычисления значений из первого шага просто игнорятся

```
repl

```hs
https://stepik.org/lesson/38579/step/14?unit=20504
TODO
{--
Те из вас, кто проходил первую часть нашего курса, конечно же помнят, последнюю задачу из него. 
В тот раз всё закончилось монадой `State`, но сейчас с неё все только начинается!

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

Вам дано значение типа `Tree ()`, иными словами, вам задана форма дерева. 
От вас требуется сделать две вещи: 
во-первых, пронумеровать вершины дерева, обойдя их `in-order` обходом (левое поддерево, вершина, правое поддерево); 
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
go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go = undefined

-- solution

```
test

## chapter 4.3, Трансформер ExceptT

https://stepik.org/lesson/38580/step/1?unit=20505

- Тип ExceptT
- Функтор ExceptT
- Аппликативный функтор ExceptT
- Монада ExceptT
- Лифтинг и стандартный интерфейс для ExceptT
- Примеры работы с ExceptT
- Исправленная версия Applicative для ExceptT



grep `TODO` markers, fix it.
