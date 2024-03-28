# FP Haskell, chapter 4, трансформеры монад

[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus)

[code sandbox](./chapter-4/test-transformers.hs)

Мотивация: рассматриваем в деталях реализацию трансформеров: WriterT, StateT, ExceptT.
Рефлексируем над проблемой лифтинга, смотрим на возможности неявного лифтинга: MultiParamTypeClasses.
Реализация трансформеров: MTL vs transformers.

definitions:
- WriterT
- StateT
- ExceptT

```hs
-- Writer

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: (Monad m)=> (a, w) -> WriterT w m a -- конструктор
writer = WriterT . return -- упаковали во внутреннюю монаду (полиморфизм), упаковали во врайтер

execWriterT :: (Monad m)=> WriterT w m a -> m w -- доступ к логу
execWriterT = (fmap snd) . runWriterT -- монада лога из монады пары получается через функтор (fmap)

instance (Functor m)=> Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . (fmap updater) . runWriterT
        where updater ~(x, log) = (f x, log)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT (pure (x, mempty)) -- вызов пюре для внутренней монады (аппликатива)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v) -- liftA2 поднимает создание новой пары во внутреннюю монаду
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)

instance (Monoid w, Monad m)=> Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= k = WriterT $ do -- ду-нотация залезает во внутреннюю монаду, внешнюю оболочку снимают метки доступа `runWriterT`
        ~(v1, w1) <- runWriterT m -- левая часть, лениво
        ~(v2, w2) <- runWriterT (k v1) -- правая часть, лениво
        return (v2, w1 `mappend` w2) -- упаковали во внутреннюю монаду
    fail :: String -> WriterT w m a -- вопрос, что делать при ошибках? ХЗ, зависит от семантики
    fail = WriterT . fail -- делегируем обработку ошибки во внутреннюю монаду (если это спиисок, то будет эффект прерывания цикла)

instance (Monoid w)=> MonadTrans (WriterT w) where
    lift :: (Monad m)=> m a -> WriterT w m a
    lift m = WriterT $ do -- нужно перепаковать значение из пришедшей монады в пару-внутри-монады
        x <- m -- вынули из пришедшей монады
        return (x, mempty) -- засунули во внутреннюю монаду, добавив пустой лог

tell :: (Monad m) => w -> WriterT w m ()
tell w = writer ((), w)

listen :: (Monad m)=> WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w) -- перепаковка структуры

censor :: (Monad m)=> (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w) -- дополнительная трансформация лога

-- State

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f) -- `return . f` завернем результат стрелки в монаду (внутреннюю)

evalStateT = (fmap fst .). runStateT
execStateT = (fmap snd .). runStateT

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> -- ключевой момент: использование внутреннего fmap для затаскивания функции во внутреннюю монаду
    fmap updater (runStateT m st) where updater ~(x, s) = (f x, s)

instance (Monad m)=> Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s) -- добавился внутремонадный `return` для заворачивания пары
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do -- появлась ду-нотация для доступа во внутреннюю монаду
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance (Monad m)=> Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do -- ду-нотация для прохода во внутреннюю монаду
    ~(x, s') <- runStateT m s -- ленивый пат.мат., левое вычисление
    runStateT (k x) s' -- правое вычисление

instance MonadTrans (StateT s) where
  lift :: (Monad m)=> m a -> StateT s m a
  lift m = StateT $ \st -> do -- залезаем во внутреннюю монаду (лямбда отражает структуру трансформера - стрелка из внешненго стейта)
    a <- m -- вынимаем значение
    return (a, st) -- перепаковываем в нужную нам пару

get :: (Monad m)=> StateT s m s
get = state $ \s -> (s, s) -- вернуть внешний стейт

put :: (Monad m)=> s -> StateT s m ()
put s = state $ \_ -> ((), s) -- положить данный стейт

modify :: (Monad m)=> (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s) -- преобразовать и положить внешний стейт

-- Except

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

except :: (Monad m) => Either e a -> ExceptT e m a
except = ExceptT . return

instance (Functor m)=> Functor (ExceptT e m) where
  fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
  fmap f = ExceptT . fmapper . runExceptT where
    fmapper = fmap (fmap f) -- фмап для ийзер и фмап для абстрактной монады эм (которая также функтор)

instance (Applicative m)=> Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right -- также, просто упаковка по слоям, пюре это внутренняя абстрактная монада эм
  f <*> v = ExceptT $ liftA2 updater (runExceptT f) (runExceptT v) where -- n.b. liftA2
    updater (Left e) _ = Left e
    updater (Right g) x = fmap g x
  -- Композиция аппликативных функторов является аппликативным функтором
  -- некорректная реализация: композиция аппликативов, противоречит семантике Either
  -- аппликатив прокидывает эффекты дальше, с помощью лифта, нам надо всё тормознуть при ошибке в левом шаге цепочки
  f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v) -- вариант с аплайд овер для аппликатива Either
-- корректная реализация, завязанная на монаду внутреннего слоя, поведение соответствует семантике Either
instance (Monad m)=> Applicative (ExceptT e m) where
  pure x = ExceptT $ pure (Right x)
  (ExceptT mef) <*> (ExceptT mea) = ExceptT $ do -- залезли в монаду нижнего слоя
    ef <- mef -- из монады в которой лежит `Either e f` вынимаем этот ийзер
    case ef of -- и в зависимости от значения типа-суммы:
      Left  e -> return $ Left e -- была ошибка, двигаем ошибку и ничего не делаем, только пакуем ошибку в монаду `m`
      Right f -> fmap (fmap f) mea -- поднимаем функцию эф через два слоя, happy path

instance (Monad m)=> Monad (ExceptT e m) where
  fail = ExceptT . fail -- делегируем обработку ошибок во внутренний слой, монаду
  m >>= k = ExceptT $ do -- залезли во внутреннюю монаду, работаем с Either
    a <- runExceptT m -- левое вычисление, a :: Either, (runExceptT m) :: Monad Either
    case a of
      Left e -> return (Left e) -- запакуем обратно в монаду
      Right x -> runExceptT (k x) -- аналогично, но наоборот, распакуем в монаду (k x :: ExceptT)

instance MonadTrans (ExceptT e) where
  lift :: m a -> ExceptT e m a
  lift = ExceptT . (fmap Right) -- поднимаем конструктор Either в монаду m

throwE :: (Monad m)=> e -> ExceptT e m a
throwE = ExceptT . return . Left -- последовательное заворачивание ошибки в слои оберток

catchE :: (Monad m)=> ExceptT e m a -> (e -> ExceptT e2 m a) -> ExceptT e2 m a
m `catchE` h = ExceptT $ do -- ошибка и функция обработки ошибки (handler), уходим в монаду
  a <- runExceptT m -- вынимаем из монады значение Either
  case a of
    Left  e -> runExceptT (h e) -- вынимаем монаду с обработанной ошибкой
    Right v -> return (Right v) -- упаковываем в монаду правильный результат

{-# LANGUAGE FunctionalDependencies #-}
class Mult a b c | a b -> c where
  (***) :: a -> b -> c

```
definitions

## chapter 4.1, Трансформер WriterT

https://stepik.org/lesson/38578/step/1?unit=20503

- Тип WriterT
- Функтор WriterT
- Аппликативный функтор WriterT
- Монада WriterT
- Лифтинг в WriterT
- Стандартный интерфейс для WriterT
- Совместное использование ReaderT и WriterT

### 4.1.2 newtype WriterT, writer, execWriterT

Writer: "запись в лог", сделано как пара (тупль) из (значения, моноида "лога").

Моноид: нейтральный элемент и ассоциативная бинарная операция.

Реализуем трансформер `WriterT`
```hs
import Control.Applicative (liftA2) -- даух-параметрическая функция в аппликатив
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

writer :: (Monad m)=> (a, w) -> WriterT w m a
writer = WriterT . return -- упаковали во внутреннюю монаду (полиморфизм), упаковали во врайтер

runWriterT (writer (42, "foo")) :: [(Int, String)] -- сняли полиморфизм, явно указав тип внутренней монады

-- доступ к логу, если значение надо проигнорировать
execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: (Monad m)=> WriterT w m a -> m w
execWriterT = (fmap snd) . runWriterT -- монада лога из монады пары получается через функтор (fmap)
-- зачем сделали требование к контексту: Monad?
-- liftM вместо fmap, в разных версиях библиотеки по разному
-- ибо есть версии, где монада не расширяет функтор

```
repl

### 4.1.3 Functor WriterT (lazy vs eager)

Реализуем функтор для трансформера `WriterT`
```hs
instance Functor (Writer w) where -- было
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f = Writer . updater . runWriter -- развернуть из оболочки, сгенерить новую пару, завернуть обратно в оболочку
        where updater ~(x, log) = (f x, log) -- ленивый пат.мат. `~`, irrefutable pattern
-- пара это функтор, но для обратного порядка элементов,
-- поэтому мы не можем здесь просто прокинуть fmap в пару, надо ручками реализовать

-- в стандартной либе есть обе версии функтора: ленивый и энергичный,
-- ленивый может падать по памяти из-за накопления thunk-ов, энергичный может падать на undefined или бесконечности

instance (Functor m)=> Functor (WriterT w m) where
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
instance (Monoid w)=> Applicative (Writer w) where -- было
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    f <*> v = Writer $ updater (runWriter f) (runWriter v)
        where updater ~(g, w1) ~(x, w2) = (g x, w1 `mappend` w2)
-- распаковать, сгенерить новую пару, запаковать

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT (pure (x, mempty)) -- вызов пюре для внутренней монады (аппликатива)

    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v) -- liftA2 поднимает создание новой пары во внутреннюю монаду
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

### 4.1.5 test

```hs
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

-- solution
-- «приведут к расходимостям при вычислении до нормальной формы»

- fst . runWriter $ take 5 <$> sequenceA (repeat actionLazy)
нет расходимости: take 5 дает нам 5 элементов из списка результатов, fst забирает этот короткий список. Ленивость не дает вылезти бесконечному логу

- fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
есть расходимость: бесконечный лог из аппликатива проявляется в силу жадности вычислений в аппликативе

- runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
есть расходимость: аналогично (fst . runStrictWriter) зависает на бесконечном логе в аппликативе

- runWriter $ take 5 <$> sequenceA (repeat actionLazy)
есть расходимость: при приведении к нормальной форме (не WHNF) получим бесконечность

seq (runWriter $ take 5 <$> sequenceA (repeat actionLazy)) () 
-- > seq считает до слабой головной нормальной формы, а мы имеем в виду вычисления до нормальной формы, то есть до значения.

{--
- repeat x is an infinite list, with x the value of every element.
- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
Evaluate each action in the structure from left to right, and collect the results.

fst . runLazyWriter $ take 5 <$> sequenceA (repeat actionLazy) -- [42,42,42,42,42]
fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict) -- hang
runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict) -- hang
runLazyWriter $ take 5 <$> sequenceA (repeat actionLazy) -- inf log

--}

run на ленивом: результат требует пару из пятиэлементного списка и лога со всеми эффектами, то есть бесконечного — расходится.

run на строгом: разойдётся уже на этапе sequenceA, ибо потребует вычисления всех значений и эффектов.

fst . run на строгом: аналогично — расходится.

fst . run на ленивом: результат требует только первый элемент пары, то есть пятиэлементный список, что вполне вычисляется, бесконечности вычислять будет лень.

```
test

### 4.1.6 Monad WriterT

Реализуем монаду для `WriterT`
```hs
instance (Monoid w)=> Monad (Writer w) where -- было
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    -- реализация так записано, чтобы было похоже на ду-нотацию (см ниже, WriterT)
    m >>= k = Writer $ let -- завернули в оболочку врайтера
        (v1, w1) = runWriter m -- распаковали левую часть бинда
        (v2, w2) = runWriter (k v1) -- вычислили стрелку Клейсли (зависимость от левой часли бинда), правая часть бинда
        in (v2, w1 `mappend` w2) -- создали новую пару

instance (Monoid w, Monad m)=> Monad (WriterT w m) where
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
[(42, "foo")]

runWriterT $ do { 555 <- writer (41, "foo"); return 42 } :: [(Int, String)]
[] -- был вызван и обработан `fail` на сломанном пат.мат. `555 <- ...`
-- очевидно, фейл обработан в монаде списка

runWriterT $ do { 1 <- WriterT [(41, "foo"), (1, "bar")]; return 42 } :: [(Int, String)]
[(42, "bar")] -- видно, что только первое вычисление в списке сломалось на плохом пат.мат.

```
repl

Разные порядки вложенности монад в трансформерах
```hs
-- логи от засохших веток исполнения не сохраняются. Кажется, что это не всегда то, что хочется получить
-- Наверное, в таком случае нужно просто поменять порядок трансформеров
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
-- мемоизацию использовал естественную для рекуррентных соотношений в  Haskell - рекурсией списка
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
-- не все так хорошо по производительности, из линейного времени доступа к элементам списка

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
 
 ### 4.1.7 test

```hs
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
instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
  fail msg = undefined

-- solution

import Control.Applicative
instance (Monad m)=> Monad (LoggT m) where
  fail = LoggT . fail
  return = pure
  m >>= k = LoggT $ do
    ~(Logged s1 x1) <- runLoggT m
    ~(Logged s2 x2) <- runLoggT (k x1)
    return $ Logged (s1 ++ s2) x2
instance (Applicative m)=> Applicative (LoggT m) where
  pure = LoggT . pure . (Logged "") -- pure x = LoggT (pure (Logged "" x))
  f <*> v = LoggT $ liftA2 updater (runLoggT f) (runLoggT v) where
    updater ~(Logged s1 g) ~(Logged s2 x) = Logged (s1 ++ s2) (g x)
instance (Functor m)=> Functor (LoggT m) where
  fmap f = LoggT . (fmap updater) . runLoggT where
    updater ~(Logged log x) = Logged log (f x)

-- alternatives

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

```
test

### 4.1.8 MonadTrans lift

Дополнительно надо реализовать `lift` для подъема мроизвольной монады в трансформер
```hs
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

instance (Monoid w)=> MonadTrans (WriterT w) where
    lift :: (Monad m)=> m a -> WriterT w m a
    lift m = WriterT $ do -- нужно перепаковать значение из пришедшей монады в пару-внутри-монады
        x <- m -- вынули из пришедшей монады
        return (x, mempty) -- засунули во внутреннюю монаду, добавив пустой лог

-- example

wl3 = WriterT $ [(1, "one"), (10, "ten"), (20, "twenty")]
runWriterT $ do { x <- wl3; f <- lift [pred, succ]; return (f x) }
[(0, "one"), (2, "one"), (9, "ten"), (11, "ten"), (19, "twenty"), (21, "twenty")]
-- для каждого значения из wl3, выполнить каждую ф. из f

```
repl

### 4.1.9 WriterT (tell, listen, censor)

Стандартный интерфейс Writer: `tell, listen, censor`
```hs
tell :: w -> Writer w ()
tell w = Writer ((), w)

tell :: (Monad m)=> w -> WriterT w m ()
tell w = writer ((), w) -- пишем в лог

-- example
runWriterT $ do { x <- wl3; f <- lift [pred, succ]; tell " foo"; return (f x) }
[(0, "one foo"), (2, "one foo"), (9, "ten foo"), (11, "ten foo"), (19, "twenty foo"), (21, "twenty foo")]

-- по аналогии с юниксовым `tee`: выдать лог в отдельный канал для побочной обработки
listen :: (Monad m)=> WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w) -- перепаковка структуры

censor :: (Monad m)=> (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w) -- дополнительная трансформация лога

-- example
runWriterT (censor (\(c:cs) -> [c]) wl3)
[(1, "o"),(10, "t"),(20, "t")]
```
repl

### 4.1.10 test

```hs
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

write2log :: (Monad m)=> String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())
type Logg = LoggT Identity
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT -- runLogg (LoggT m) = runIdentity m

-- alternatives

write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (`Logged` ())
type Logg = LoggT Identity
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (flip Logged ())
type Logg = LoggT Identity
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

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
    ReaderT [String] -- внешняя монада, трансформер, енв это список строк
    (WriterT String Identity) -- внутренняя монада (составлена из трансформера над айдентити), лог это строка
    String -- содержимое композитной монады
logFirstAndRetSecond = do
    el1 <- asks head -- ридер
    el2 <- asks (map toUpper . head . tail) -- ридер
    lift $ tell el1 -- голову записали в лог (внутренняя монада) -- райтер
    return el2

strings = ["ab", "cd", "fg"]
runIdentity $ runWriterT (runReaderT logFirstAndRetSecond strings)
("CD", "ab") -- первый элемент пары: значение из ридера, второй элемент пары: строка лога

```
repl

### 4.1.12 test

```hs
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

instance MonadTrans LoggT where
  -- lift :: (Monad m)=> m a -> LoggT m a
  lift = LoggT . fmap (Logged "")
  -- lift m = LoggT $ fmap (Logged "") m
  -- lift m = LoggT $ do
  --   x <- m
  --   return (Logged "" x)

-- alternatives

instance MonadTrans LoggT where lift m = LoggT $ Logged "" <$> m

instance MonadTrans LoggT where lift = LoggT . fmap (Logged "")

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
-- для "сложных" надо использовать родной конструктор StateT и передавать в него лямбду

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

### 4.2.4 test

```hs
{--
Реализуйте функции `evalStateT` и `execStateT`
--}
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = undefined

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = undefined

-- solution

evalStateT :: (Monad m)=> StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st -- evalStateT m s = fst <$> (TS.runStateT m s)
execStateT :: (Monad m)=> StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st

-- alternatives

evalStateT =  (fmap fst .). runStateT
execStateT = (fmap snd .). runStateT

evalStateT st s = fst <$> runStateT st s
execStateT st s= snd <$> runStateT st s

evalStateT (StateT f) = (fmap fst) . f
execStateT (StateT f) = (fmap snd) . f

```
test

### 4.2.5 test

```hs
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
-- Кака бычно, затык на лямбдах, которые вытекают из стрелочных типов. 
-- А само преобразование простое, приходит m a, вернуть надо m (a, r), 
-- т.е. внутри монады преобразовать значение в пару, достав из воздуха енв. ридера.

readerToStateT :: (Monad m)=> ReaderT r m a -> StateT r m a
readerToStateT (ReaderT k) = StateT (\st -> convert st) where
  convert st = (\x -> (x, st)) <$> (k st) -- (s -> m a) -> (s -> m (a, s))
-- readerToStateT rt = TS.StateT $ \s -> do
--   let ma = TR.runReaderT rt s
--   a <- ma
--   return (a, s)

-- alternatives

readerToStateT (ReaderT f) = StateT $ \s -> do { x <- f s; return (x, s) }

readerToStateT = StateT . (fmap <$> flip (,) <*>) . runReaderT -- жонглирование стрелками, охуенчик

readerToStateT rd = StateT fun where fun r = (\a -> (a, r)) <$> runReaderT rd r

readerToStateT (ReaderT rr) = StateT $ \s -> fmap (flip (,) s) (rr s)
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
  fmap f m = StateT $ \st -> -- ключевой момент: использование внутреннего fmap для затаскивания функции во внутреннюю монаду
    fmap updater (runStateT m st) where updater ~(x, s) = (f x, s)

ghci> :t runStateT
runStateT :: StateT s m a -> s -> m (a, s) -- функция двух аргументов,
-- берет трансформер, начальный стейт, возвращает пару-в-монаде

-- example

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)]
ghci> runStateT sl3 5 -- в объекте sl3 стрелка, скормив ей начальный стейт 5 получаем:
[(6,42), (7,5), (8,10)]

ghci> runStateT (fmap (^2) sl3) 5 -- начальный стейт = 5, функторим возведенеи в квадрат
[(36,42), (49,5), (64,10)] -- (5+1)^2, (5+2)^2, (5+3)^2

-- заметим, функтор может менять значения, но не может менять стейт или структуру
```
repl

### 4.2.7 Applicative State

Реализуем аппликативный функтор для трансформера `StateT`.
Аппликатив уже может работать со стейтом.
Кстати, тут стейт прокидывается (и модифицируется) по цепочке (в отличие от ридера).

Аппликатив не может работать со структурой: влияние левого вычисления на правое и структуру результата это работа монады.
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
instance (Monad m)=> Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s) -- добавился внутремонадный `return` для заворачивания пары

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do -- появлась ду-нотация для доступа во внутреннюю монаду
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')
-- обратите внимание: заменить контекст (Monad m) на (Applicative m) не получается,
-- интерфейс аппликатива не дает возможности прокинуть эффект (повлиять) на второе вычисление,
-- только монада может позволить левому вычислению влиять на правое

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

### 4.2.9 test

```hs
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
instance (Monad m)=> Applicative (StateT s m) where
Видим, что Аппликатив-стейт возможен при условии, что внутри монада.

> на примере одного из случаев:
StateT () (ReaderT Int (Writer String)) - как следует из последних минут предыдущего видео, 
чтобы этот конструктор был аппликативом, ReaderT Int (Writer String) должен быть монадой.
Чтобы ReaderT Int (Writer String) был монадой, нужно, чтобы Writer String был монадой (смотри лекцию 4.1 Трансформер WriterT)
чтобы Writer String был монадой, нужно, чтобы String был моноидом

> StateT имеет несколько параметров и объявлен представителем Applicative (и Monad) не безусловно, 
а при некоторых дополнительных условиях на некоторые из этих параметров
--}

+ StateT Int (Writer (Sum Int))
+ StateT () (ReaderT Int (Writer String))
- StateT () (StateT Int (Const ()))
- StateT Int ZipList
- StateT () (ReaderT Int ZipList)
- StateT String (Const Int)

-- трансформеру нужна монада внутри, только два варианта могут в такое ограничение

type X1 = TS.StateT Int (TW.Writer (Sum Int)) -- yes, райтер над моноидом
type X3 = TS.StateT () (TR.ReaderT Int (TW.Writer String)) -- yes

type X2 = TS.StateT () (TS.StateT Int (Const ())) -- no, нет монады над Конст но она нужна стейту
type X4 = TS.StateT Int ZipList -- no, нет монады над зиплист но она нужна стейту
type X5 = TS.StateT () (TR.ReaderT Int ZipList) -- no, аналогично
type X6 = TS.StateT String (Const Int) -- no, аналогично

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
    in  runState (k x) s2 -- правое вычисление, с учетом результатов левого (стейт можно поменять в зависимости от значения в левой м.)

-- для трансформера стало так:
instance (Monad m)=> Monad (StateT s m) where
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

### 4.2.11 test

```hs
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
  fail = lift . fail where lift m = StateT (\st -> (\x -> (x, st)) <$> m) -- solution
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

-- alternative

fail = StateT . const . fail

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

### 4.2.13 StateT (get, put, modify)

Реализуем стандартный интерфейс: `get, put, modify`
```hs
-- запаковка переданного снаружи стейта в пару и полиморфную монаду (через конструктор `state`)
get :: (Monad m)=> StateT s m s
get = state $ \s -> (s, s) -- вернуть внешний стейт

-- аналогично `get` но внешний стейт игнорится а явно переданный упаковывается
put :: (Monad m)=> s -> StateT s m ()
put s = state $ \_ -> ((), s) -- положить данный стейт

-- аналогично, но теперь есть функция преобразования стейта, преобразованный внешний стейт запаковывается
modify :: (Monad m)=> (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s) -- преобразовать и положить внешний стейт

-- Никакого взаимодействия в внутренней монадой, работаем только со стейтом;

-- example

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)] -- внутренняя монада списка
ghci> runStateT (do {sl3; y <- get; put (y - 2); return (2 * y) }) 5
[(84,40), -- 2*42, 42-2
(10,3),   -- 2*5, 5-2
(20,8)]   -- 2*10, (5*2)-2
-- ретурн кладет в первое значение пары `2 * y`
-- где `y` это стейт из предыдущего шага пайплайна (get дает нам стейт)
-- в итоге вычисления значений из первого шага просто игнорятся
```
repl

### 4.2.14 test

```hs
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
go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go = undefined

-- solution

-- Нам надо уметь отделять листы от форков, ибо: количество листов и нумерация нод.
go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go = traverse_ withState where
  traverse_ f (Leaf x) = let res = Leaf <$> (f x) in res <* do { lift $ tell (Sum 1) }
  traverse_ f (Fork l x r) = Fork <$> (traverse_ f l) <*> (f x) <*> (traverse_ f r)
  withState _ = do          -- _ это юнит, игнорим
    n <- get                -- взять текущий номер из стейта
    modify succ             -- следующий номер положить в стейт
    -- lift $ TW.tell (Sum 1)  -- добавить в лог врайтера 1 (но только если это лист!)
    return n                -- номер узла

-- alternatives

go (Leaf _)     = lift (tell (Sum 1)) >> Leaf <$> pick
go (Fork l m r) = Fork <$> go l <*> pick <*> go r
pick = state ((,) <$> id <*> (+1))

go (Leaf _)            = liftM  Leaf step <* lift (tell $ Sum 1)
go (Fork left _ right) = liftM3 Fork (go left) step (go right)
step                   = get <* modify succ

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

### 4.3.2 newtype ExceptT

Монада Except, с эффектом обработки ошибочных ситуаций (под капотом Either).
Трансформер для этой монады, соответственно, ExceptT.

Будем реализовывать логику иксепта, навернутого на произвольную монаду.
Как и ранее, опираясь на ранее рассмотренную логику монады Except
```hs
import Control.Applicative (liftA2)

newtype Except e a = Except { runExcept :: Either e a } -- двух-параметрический конструктор

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- довольно просто, нет стрелочных типов, есть тип-сумма Either, завернутый в абстрактную монаду

-- тривиальный конструктор, берет ийзер и заворачивает его в монаду и в иксептТ
except :: (Monad m) => Either e a -> ExceptT e m a
except = ExceptT . return

-- example

ghci> runExceptT (except $ Right 42) :: [Either String Int]
[Right 42]
-- если не подсказать типы компайлеру, то сам он их не выведет: тип монады неизвестен, как и тип ошибки
ghci> :t runExceptT (except $ Right 42)
runExceptT (except $ Right 42)
  :: (Monad m, Num a) => m (Either e a)

```
repl

### 4.3.3 Functor ExceptT

Реализуем функтор для ExceptT
```hs
instance Functor (Except e) where -- связали один параметр, второй свободен
  fmap :: (a -> b) -> Except e a -> Except e b
  fmap f = Except . (fmap f) . runExcept -- тип не стрелочный, лямбд не будет. Будет опора на факт, что Either это функтор.
  -- логика трамвайная: распаковали эйзер, подняли фмапом туда функцию, результат заапаковали обратно в иксепт.

instance (Functor m)=> Functor (ExceptT e m) where
  fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
  fmap f = ExceptT . fmapper . runExceptT where fmapper = fmap (fmap f)
  -- фмап для ийзер и фмап для абстрактной монады эм (которая также функтор)

-- example

ghci> runExceptT (fmap (+9) (except $ Right 42)) :: [Either String Int]
[Right 51]

ghci> runExceptT (fmap undefined (except $ Left "foo")) :: [Either String Int]
[Left "foo"] -- для случая "ошибки" никакой обработки нет, ошибка протаскивается по цепочке
-- функция (андефайнед) даже и не вызывается

```
repl

### 4.3.4 Applicative ExceptT

Реализуем аппликатив для ExceptT
```hs
instance Applicative (Except e) where
  pure = Except . Right -- просто упаковка значения в слои обвязок

-- f и v это стрелка и значение параметра для стрелки, упакованные в слои Either и Except
-- поэтому вся логика заключается в снятии и накручивании слоев
  f <*> v = Except $ updater (runExcept f) (runExcept v) where
    updater (Left e) _ = Left e -- с этим понятно, пропихиваем ошибку
    updater (Right g) x = fmap g x -- икс это Either, который функтор, поэтому фмап
-- если посмотреть внимательно, то можно заметить, что апдейтер повторяет реализацию оператора "аплайд овер"
-- для аппликатива Either. Поэтому можно сделать так:
  f <*> v = Except $ (runExcept f) <*> (runExcept v) -- вынули Either и сделали "f applied over v"

instance (Applicative m)=> Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right -- также, просто упаковка по слоям, пюре это внутренняя абстрактная монада эм
  -- Не настораживает безусловное применение только одного из двух конструкторов? Правильно настараживает, подробности позже ...

  f <*> v = ExceptT $ liftA2 updater (runExceptT f) (runExceptT v) where -- n.b. liftA2
    updater (Left e) _ = Left e
    updater (Right g) x = fmap g x
-- почему это один-в-один повторяет Applicative Except?
-- потому, что лифт поднимает этот код в промежуточный слой этой нашей абстрактной монады,
-- получается красиво, лифт это круто
  f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v) -- вариант с аплайд овер для аппликатива Either
-- `(<*>) (runExceptT f) (runExceptT v)` вот это поднимается лифтом в абстрактрую монаду (здесь аппликатив)

-- example: эффект ошибки есть, эффект внутренней монады (списка) есть. работает

ghci> runExceptT $ (except $ Right (^2)) <*> (except $ Right 3) :: [Either String Int]
[Right 9]
ghci> runExceptT $ (except $ Left "foo") <*> (except $ Right 3) :: [Either String Int]
[Left "foo"]
ghci> runExceptT $ (except $ Right (^2)) <*> (except $ Left "bar") :: [Either String Int]
[Left "bar"]

```
repl

Композиция аппликативных функторов является аппликативным функтором.
Но как мы увидим позднее, такая реализация "не совсем хорошая", в стдлиб сделано иначе.
Интрига.

### 4.3.5 Monad ExceptT

Реализуем монаду ExceptT
```hs
instance Monad (Except e) where
  m >>= k = Except $ case runExcept m of -- левое вычисление
    Left e -> Left e -- пропихиваем ошибку
    Right x -> runExcept (k x) -- ошибки слева нет, запускаем стрелку Клейсли (правое вычисление)

instance (Monad m)=> Monad (ExceptT e m) where
  m >>= k = ExceptT $ do -- залезли во внутреннюю монаду, работаем с Either
    a <- runExceptT m -- левое вычисление, a :: Either, (runExceptT m) :: Monad Either
    case a of
      Left e -> return (Left e) -- запакуем обратно в монаду
      Right x -> runExceptT (k x) -- аналогично, но наоборот, распакуем в монаду (k x :: ExceptT)
    
  fail = ExceptT . fail -- делегируем обработку ошибок во внутренний слой, монаду
  -- потому что ничего содержательного здесь мы сделать не можем

-- example

ghci> runExceptT $ do { f <- except $ Right (^2); x <- except $ Right 3; return $ f x } :: [Either String Int]
[Right 9] -- иллюстрация хеппи-пас для байнда

```
repl

### 4.3.6 test

```hs
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
moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves = undefined

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie = undefined

-- solution
Подсказка (видимо) говорит о том, что трансформер ExceptT над монадой списка, это (под капотом)
список Either-ов, что и требуется на выходе moves.
Т.е. работу можно выполнить в монаде трансформера, что по факту будет в монаде списка.

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
    Left e -> [ep] -- приехали
    Right p -> moves_ gmap (steps-1) next4 where
      next4 = (gamePoint gmap) <$> (fourWays p)

fourWays (x, y) = [p1, p2, p3, p4] where
  p1 = (x+1, y)
  p2 = (x-1, y)
  p3 = (x, y+1)
  p4 = (x, y-1)

gamePoint :: GameMap -> Point -> Either DeathReason Point
gamePoint g p = case g p of
  Floor -> Right p
  Chasm -> Left Fallen
  Snake -> Left Poisoned

-- alternatives

-- предполагаемый способ? через трансформер ExceptT над списком?
-- Я разобрал как это работает, всё понятно. Но я даже повторить это не смогу, у меня мозги по другому работают :(
import qualified Control.Monad.Except as E
import           Control.Monad.Trans
moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves game = (E.runExceptT . ) .  go  where

  go 0 p = verify p -- go nSteps, point: обработка последней точки
  go k p = steps p >>= go (k - 1) -- пайплайн: одна-пойнт и следом остаток шагов (где спрятаны следующие точки?)
  -- в левой части формируется список 4 точек (или сушняк-смерть), в правой части запускается обработка,
  -- фактически это итератор на рекурсии

  steps p = verify p >> lift (neighbors p) -- отбрасывает значение левого и поднимает в трансформер список 4 следующих точек
  -- левая часть нужна чтобы "засушить" умершие ветки

  verify :: Point -> E.ExceptT DeathReason [] Point -- под капотом список изеров, что и требуется
  verify p = case game p of -- в монаде трансформера иксепт-над-списком, внутри это список изеров
    Floor -> return p -- пойнт завернутый в монаду-трансформер, выйдет список одного элемента
    Chasm -> E.throwError Fallen -- засушили ветку, вычисления в монаде далее не пойдут
    Snake -> E.throwError Poisoned

  neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r = (((length . filter (== Left r)).).). moves

--------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n start | n < 0 = []
                | n == 0 = [Right start] -- possible bug, should be mapping Point -> Either
                | otherwise = do -- list monad
                      np <- move start -- NextPoint iteration
                      case m np of
                           Snake -> [Left Poisoned] -- stop here
                           Chasm -> [Left Fallen]
                           Floor -> moves m (n-1) np -- next move, recursion

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r m n start = (length . filter (== r) . lefts) (moves m n start)
-- lefts :: [Either a b] -> [a] -- base Data.Either -- Extracts from a list of Either all the Left elements

move :: Point -> [Point] -- produce next 4 points
move (x, y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

--------------------------------------------------------------------------------

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves mp n ini = runExceptT allSteps where
  -- создание цепочки из nSteps вычислений в монаде иксептТ (список изеров),
  -- где в левой части формируется список точек для обработки правой частью
  allSteps = foldl (>>=) (ExceptT [Right ini]) (replicate n doStep)
  doStep p = do -- монада трансформера иксептТ, посчитает результат для 4 соседей точки
    np <- lift $ nextStepPoints p -- перебор точек из списка
    case (mp np) of
      Snake -> throwE Poisoned -- засушили
      Chasm -> throwE Fallen
      Floor -> return np -- список одной точки (тут можно было бы сделать рекурсию)

  nextStepPoints (a, b) = [(a, b - 1), (a, b + 1), (a - 1, b), (a + 1, b)]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie reason mp n ini = length $ filter (== Left reason) $ moves mp n ini

-----------------------------------------------------------------------------------

throwE = ExceptT . return . Left
lift = ExceptT . fmap Right

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n ini = runExceptT $ iterate move (return ini) !! n
  where
    move :: ExceptT DeathReason [] Point -> ExceptT DeathReason [] Point
    move p = do
      (x, y) <- p
      p' <- lift [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      case m p' of
        Chasm -> throwE Fallen
        Snake -> throwE Poisoned
        Floor -> return p'

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr = (((length . filter (Left dr ==)) .).) . moves

----------------------------------------------------------------------------------

movesFrom :: Point -> [Point]
movesFrom (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n p = case m p of
  Floor -> if n > 0 then concatMap (moves m (n-1)) (movesFrom p) else [Right p]
  Chasm -> [Left Fallen]
  Snake -> [Left Poisoned]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r m n p = length $ filter (either (==r) (const False)) $ moves m n p

----------------------------------------------------------------------------------

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n p = runExceptT $ moves' n p where
    moves' 0 p = ExceptT $ [Right p]
    moves' n p = do
        (x,y) <- moves' (n-1) p
        p' <- ExceptT $ fmap Right [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
        case m p' of
            Floor -> return p'
            Chasm -> ExceptT $ [Left Fallen]
            Snake -> ExceptT $ [Left Poisoned]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie d m n p = length $ filter (== Left d) $ moves m n p

---------------------------------------------------------------------------------

result :: GameMap -> Point -> ExceptT DeathReason [] Point
result gm p = case gm p of
  Floor -> ExceptT [Right p]
  Chasm -> ExceptT [Left Fallen]
  Snake -> ExceptT [Left Poisoned]
  
neighbors :: Point -> ExceptT DeathReason [] Point
neighbors (x,y) = ExceptT $ map Right [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gm n p = runExceptT $ (foldr (\f g x -> f x >>= g) return $ take n $ repeat fun) p where
  fun p1 = neighbors p1 >>= result gm

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr gm n p = length $ filter (Left dr ==) $ moves gm n p

-----------------------------------------------------------------------------------

import Control.Monad.Trans.Class

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m n p = runExceptT $ move n p
  where
    move 0 p = pure p
    move i (x, y) = do
      p' <- lift $ do
        dx <- [-1, 0, 1]
        dy <- [-1, 0, 1]
        guard $ abs dx /= abs dy
        pure (x+dx, y+dy)
      case m p' of
        Snake -> throwE Poisoned
        Chasm -> throwE Fallen
        Floor -> move (i-1) p'

instance MonadTrans (ExceptT e) where
  lift m = ExceptT $ do
    a <- m
    pure $ pure a

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . pure . Left

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r m i p = length . filter (== Left r) $ moves m i p

-----------------------------------------------------------------------

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves _ n _ | n < 0 = []
moves _ 0 p = [Right p]
moves fp n (x, y) = do 
    p <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    case fp p of
        Chasm -> [Left Fallen]
        Snake -> [Left Poisoned]
        Floor -> moves fp (n-1) p
   
waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r = (((length . filter (Left r ==)) . ) . ) . moves

```
test

### 4.3.7 lift, throwE, catchE

Реализуем стандартный интерфейфс монады ExceptT
```hs
instance MonadTrans (ExceptT e) where
  lift :: m a -> ExceptT e m a
  lift = ExceptT . (fmap Right) -- поднимаем конструктор Either в монаду m

throwE :: (Monad m)=> e -> ExceptT e m a
throwE = ExceptT . return . Left -- последовательное заворачивание ошибки в слои оберток

catchE :: (Monad m)=> ExceptT e m a -> (e -> ExceptT e2 m a) -> ExceptT e2 m a
m `catchE` h = ExceptT $ do -- ошибка и функция обработки ошибки (handler), уходим в монаду
  a <- runExceptT m -- вынимаем из монады значение Either
  case a of
    Left  e -> runExceptT (h e) -- вынимаем монаду с обработанной ошибкой
    Right v -> return (Right v) -- упаковываем в монаду правильный результат

```
repl

### 4.3.8 test

```hs
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
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

{- Не снимайте комментарий - эти объявления даны в вызывающем коде
newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
-}

getValidPassword :: PwdErrorIOMonad String
getValidPassword = undefined

-- solution

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)
import Data.Either ( lefts, isLeft, )
import Control.Monad ( liftM, mplus, when, )
getValidPassword :: PwdErrorIOMonad String -- TE.ExceptT PwdError IO String -- PwdError = String
-- т.е. имеем ио монаду в которой изер: ошибка=строка, значение=строка
getValidPassword = do
  s <- liftIO getLine
  let errOrpass = check s
  when (isLeft errOrpass) (runError errOrpass)
  return s
-- runError :: (MonadIO m)=> Either PwdError a -> TE.ExceptT PwdError m b
runError (Right _) = undefined
runError (Left (PwdError msg)) = do
  liftIO (putStrLn $ "Incorrect input: " ++ msg)
  throwE (PwdError msg)
-- check :: String -> Either PwdError String
check s
  | length s < 8              = Left $ PwdError "password is too short!"
  | not (any isNumber s)      = Left $ PwdError "password must contain some digits!"
  | not (any isPunctuation s) = Left $ PwdError "password must contain some punctuation!"
  | otherwise                 = Right s
instance Monoid PwdError where mempty = PwdError mempty; (PwdError x) `mappend` (PwdError y) = PwdError $ x ++ y

-- alternatives

getValidPassword = check `catchE` handleError where -- ожидаемое решение, throw/catch
  check = do
    s <- liftIO getLine
    isValid s
    return s
  handleError (PwdError e) = do 
    liftIO $ putStrLn ("Incorrect input: " ++ e)
    throwE $ PwdError e
instance Monoid PwdError where  mempty = PwdError "";   (PwdError e1) `mappend` (PwdError e2) = PwdError (e1 ++ e2)
isValid :: String -> PwdErrorIOMonad ()
isValid s
  | length s < 8              = throwE $ PwdError "password is too short!"
  | not $ any isNumber s      = throwE $ PwdError "password must contain some digits!" 
  | not $ any isPunctuation s = throwE $ PwdError "password must contain some punctuation!" 
  | otherwise                 = return ()

getValidPassword = do
  s <- liftIO getLine
  either ((>>) <$> liftIO . putStrLn <*> (throwE . PwdError)) (return $ validatePass s)
validatePass :: String -> Either String String
validatePass s = left ("Incorrect input: " ++) $ do
  unless (length s >= 8)       $ Left "password is too short!"
  unless (any isNumber s)      $ Left "password must contain some digits!"
  unless (any isPunctuation s) $ Left "password must contain some punctuation!"
  return s

getValidPassword = do
    s <- liftIO getLine
    unless (length s >= 8) (showE "Incorrect input: password is too short!")
    unless (any isNumber s) (showE "Incorrect input: password must contain some digits!")
    unless (any isPunctuation s) (showE "Incorrect input: password must contain some punctuation!")
    return s
  where showE s = do
          liftIO $ putStrLn s
          throwE (PwdError s)
instance Monoid PwdError where   mempty  = undefined;   mappend = undefined

instance Monoid PwdError where  mempty = PwdError "";   mappend (PwdError a) (PwdError b) = PwdError $ a ++ b
getValidPassword = do
  s <- liftIO getLine
  catchE (validate s) (\(PwdError a) -> do liftIO $ putStrLn a; throwE $ PwdError a)
validate :: String -> PwdErrorIOMonad String
validate s = do
  when (length s < 8) $ throwE $ PwdError "Incorrect input: password is too short!"
  when (not $ any isNumber s) $ throwE $ PwdError "Incorrect input: password must contain some digits!"
  when (not $ any isPunctuation s) $ throwE $ PwdError "Incorrect input: password must contain some punctuation!"
  return s

```
test

### 4.3.9 тестируем реализацию (демо неудачной реализации аппликатива)

Поиграем с трансформерами, завернем в ExceptT монаду State,
которая, в свою очередь сделана через трансформер StateT накрученный на Identity
```hs
import Control.Monad.Identity
test :: (Num s)=> ExceptT e (StateT s Identity) a -> (Either e a, s)
test f = runIdentity (runStateT (runExceptT f) 3)
-- 3 это начальный стейт
-- f это монада ExceptT, переданная снаружи
-- функция тест распаковывает обертки, читать надо как "в последнюю очередь снимаем обертку айдентити"
-- т.е. айдентити самая внутренняя обертка, самая внешняя: иксепт.
-- ExceptT e (State s a) -- т.е. это внутри это так: m e a = State Either s = (Either e a, s)

-- (except $ Right 42) >> lift (put 5)
ghci> :t (except $ Right 42) >> (lift $ put 5)
  :: (Monad m, Num s)=> ExceptT e (StateT s m) () -- т.е. внутри это так: m e a = State Either s = (Either e a, s)
-- подготовка: тестовая монада иксепт, получена "правым" байндом монады `Right 42` в конструкторе иксепта, монада не указана,
-- и монады `lift (put 5)` полученной лифтом монады стейта (пут) в монаду иксептТ
-- не определены: тип ошибки и конкретизация монады внутри стейтТ

ghci> :t test $ (except $ Right 42) >> (lift $ put 5)
test $ (except $ Right 42) >> (lift $ put 5)
  :: Num s => (Either e (), s) -- `put 5`  в качестве значения дает юнит `()`
-- правый байнд игнорирует занчение левого выражения
ghci> test $ (except $ Right 42) >> (lift $ put 5)
(Right (),5)

ghci> test $ (except $ Right 42) >> (lift $ modify (+1))
(Right (),4) -- старый стейт 3 стал 4

ghci> test $ throwE "foo" >> (lift $ modify (+1))
(Left "foo",3) -- ошибка в левой части байнд была прокинута далее без изменений, 
-- ни лог ни значение правой стрелкой не поменялись

ghci> test $ lift (modify (+2)) >> throwE "foo" >> (lift $ modify (+1))
(Left "foo",5) -- до ошибки вычисления происходят и сохраняются в стейте

-- как ранее говорили, реализация аппликатива через композицию аппликативов,
-- не очень хорошая, смотрите:
ghci> test $ lift (modify (+2)) *> throwE "foo" *> (lift $ modify (+1))
(Left "foo",6) -- а должно быть `(Left "foo",5)`
-- ибо по семантике, после ошибки следующие вычисления должны игнориться

-- для справки:

instance (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  f <*> v = ExceptT $
    liftA2 -- вот тут фигня получается, вычисление поднимается в монаду (в нашем случае аппликатив State)
      updater (runExceptT f) (runExceptT v) where
        updater (Left e) _ = Left e
        updater (Right g) x = fmap g x

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)
  f <*> v = StateT $ \s -> do -- в данном случае монада айдентити
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'') -- и возвращаем измененный стейт, вот он, негодяй, из которого выходит `6`

```
repl

### 4.3.10 корректный Applicative ExceptT (без liftA2)

Аппликатив построен так, чтобы эффекты не зависили от значения,
цепочка вычислений в аппликативе тупо накапливает эффекты.
В монаде это не так, монада отличается от аппликатива: эффект второго вычисления зависит от
результата первого вычисления (левый - правый в операторе байнд).

В трансформере та же история, для обоих слоев, в аппликативе эффекты накапливаются,
не могут взаимодействовать.

Это противоречит семантике `Except`: при возникновении ошибки цепочка прерывается и дальше идет ошибка.

Поэтому надо потребовать, чтобы внутренний слой в аппликативе `ExceptT` был монадой,
тогда мы можем повлиять на эффекты.
```hs
-- предыдущая некорректная реализация: композиция аппликативов, противоречит семантике Either
instance (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  f <*> v = ExceptT $ liftA2 <*> (runExceptT f) (runExceptT v) 

-- реализация завязанная на монаду внутреннего слоя, поведение соответствует семантике Either
instance (Monad m)=> Applicative (ExceptT e m) where
  pure x = ExceptT $ pure (Right x)
  (ExceptT mef) <*> (ExceptT mea) = ExceptT $ do -- залезли в монаду нижнего слоя
    ef <- mef -- из монады в которой лежит `Either e f` вынимаем этот ийзер
    case ef of -- и в зависимости от значения типа-суммы:
      Left  e -> return $ Left e -- ошибка, двигаем ошибку и ничего не делаем, только пакуем ошибку в монаду `m`
      Right f -> fmap (fmap f) mea -- поднимаем функцию эф через два слоя, happy path

-- demo

test f = runIdentity (runStateT (runExceptT f) 3)
ghci> :t test
test :: Num s => ExceptT e (StateT s Identity) a -> (Either e a, s)

ghci> test $ lift (modify (+2)) >> throwE "foo" >> (lift $ modify (+1))
(Left "foo",5) -- корректный результат, после ошибки ничего не делали со стейтом

-- предыдущая реализация аппликатива через композицию аппликативов,
-- не очень хорошая, смотрите:
-- ghci> test $ lift (modify (+2)) *> throwE "foo" *> (lift $ modify (+1))

-- корректный результат
ghci> test $ lift (modify (+2)) *> throwE "foo" *> (lift $ modify (+1))
(Left "foo",5)

```
repl

### 4.3.11 test

```hs
{--
Попробуйте предположить, каким ещё трансформерам для реализации правильного представителя класса `Applicative` не достаточно, 
чтобы внутренний контейнер был лишь аппликативным функтором, а нужна полноценная монада?

Select all correct options from the list

- MaybeT
- ValidateT
- IdentityT
- ReaderT
- WriterT
- StateT
- ListT
--}

-- solution

- MaybeT -- более бледная версия Either, рассуждения такие же как про ExceptT

- StateT -- на лекциях рассматривали, но почему внутренний слой должен быть монадой:
-- вроде потому, что сложная внутренняя структура `s -> (a, s)` не доступна внутри аппликатива?
-- Или потому, что семантика требует прокидывания стейта, полученного (измененного) левым вычислением,
-- в правое вычисление? В аппликативе такое не возможно по определению. Думаю да, такой ответ правильный.

instance (Monad m)=> Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

```
test

### 4.3.12 test

```hs
{--
Вспомним функцию `tryRead`:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a

Измените её так, чтобы она работала в трансформере `ExceptT`
--}
tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead = undefined

-- solution
-- > Вспомним функцию tryRead
-- Из задачи 3.1.8

import Text.Read ( readMaybe, readEither, )
tryRead :: (Read a, Monad m)=> String -> ExceptT ReadError m a
tryRead "" = throwE EmptyInput
tryRead s = parse `catchE` errHandler where
  parse = except (readEither s)
  errHandler _ = throwE (NoParse s)

-- alternatives

tryRead s | null s = throwE EmptyInput
          | otherwise = case reads s of
            (x, []): _ -> return x
            _          -> throwE $ NoParse s

tryRead [] = throwE EmptyInput                                                  
tryRead s  = maybe (throwE (NoParse s)) return (readMaybe s) 

tryRead "" = throwE  EmptyInput
tryRead str =
    let aMaybe = readMaybe str
    in case aMaybe of
      (Just a) -> return a
      Nothing -> throwE (NoParse str)

tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)

tryRead str
  | null str = throwE EmptyInput
  | otherwise = case reads str of
      [(n,"")] -> return n
      _        -> throwE $ NoParse str

```
test

### 4.3.13 test

```hs
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
go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go = undefined

-- solution
-- это продолжение задачек на дерево, ранее реализованное на сервере уже есть, повторно копипастить не надо

go :: String -> ExceptT ReadError (Writer (Sum Integer)) () -- (sum-int, either-err/unit)
go s = do
  n <- tryRead s -- :: TE.ExceptT ReadError (TW.Writer (Sum Integer)) Integer
  lift $ tell (Sum n)

-- alternatives

go s = tryRead s >>= lift . tell . Sum
go s = (lift . tell . Sum ) =<< tryRead s
go = tryRead >=> (lift . tell . Sum)

```
test

## chapter 4.4, Неявный лифтинг

https://stepik.org/lesson/38581/step/1?unit=20506

- Проблема лифтинга
- Мультипараметрические классы типов
- Функциональные зависимости
- Класс типов для стандартного интерфейса
- Класс типов MonadWriter
- Представители класса MonadWriter
- Интерфейсы для доступа к WriterT с неявным лифтингом и без него

### 4.4.2 проблема с `lift (tell ...)`

Мы разобрали либу `transformers`, но настоящие профи работают с либой `MTL`,
надстройкой над трансформерами. Там есть некоторые удобства ...
```hs
-- подготовим демо, используя реализованные нами монады
type SiWs = StateT Integer (WriterT String Identity)
-- state int, writer string: стейт (инт) поверх врайтера (стринг)
-- врайтер (a, w) завернут внутрь стейта s -> (a, s)
-- это кодируется как данные стейта внутри пары врайтера
-- ((value, state), log); плюс, на самом деле, стрелка из начального стейта

test :: SiWs () -- значение: юнит (пустой тупль)
test = do
  x <- get -- взяли стейт
  when (x > 100) (lift $ tell "Overflow") -- записали в лог (через лифт), если условие сработало
  put 42 -- положили стейт (в любом случае)

-- `when :: Applicative f => Bool -> f () -> f ()`
-- ранее участвовал при изучении CPS, `callCC` использовал эту функцию для "прерывания",
-- но, вообще-то, `when` просто выполняет второе выражение при условии в первом:

runSiWs :: SiWs a -> Integer -> ((a, Integer), String)
runSiWs m = runIdentity . runWriterT . (runStateT m)
-- ((a, Integer), String): (a, Integer): state как пара
-- (pair, String): writer, лог как строка
-- SiWs a -> Integer : два параметра, наша матрешка-монада и начальный стейт

ghci> runSiWs test 33
(((),42),"") -- пустой лог-строка, в стейт положили 42

ghci> runSiWs test 133
(((),42),"Overflow") -- в лог попала запись

-- вроде работает как положено, но мы недовольны,
test = do
  x <- get
  when (x > 100) (lift $ tell "Overflow") -- вызов tell возможен только если знать устройство матрешки
  put 42
-- мы хотим иметь get, put, tell на верхнем уровне
-- это можно сделать, используя расширения языка Хаскелл
```
repl

### 4.4.3 class Mult a b c (MultiParamTypeClasses)

Мульти-параметрические классы типов (тайпклассы). Расширение `MultiParamTypeClasses`.

Вообще, тайпкласс имеет один параметр всегда, это описание конструктора типов с одним параметром.
Именно так реализуется ад-хок полиморфизм.

При нескольких параметрах, тайп-класс должен как-то описывать связть между типами, представленными этими параметрами.
```hs
-- разберем пример: умножение двух чисел разных типов (e.g. int double)
{-# LANGUAGE MultiParamTypeClasses #-}

ghci> :t (*)
(*) :: Num a => a -> a -> a -- мономорфный оператор

infixl 7 ***

class Mult a b c where -- мульти-параметрический тайп-класс
  (***) :: a -> b -> c -- сделаем его пригодным для умножения разных типов чисел

instance Mult Int Int Int where
  (***) = (*)

instance Mult Double Double Double where
  (***) = (*)

instance Mult Int Double Double where
  i *** d = fromIntegral i * d

instance Mult Double Int Double where
  d *** i = d * fromIntegral i

ghci> x = 2 :: Int
ghci> y = pi :: Double

ghci> x *** y :: Double
6.283185307179586

```
repl

### 4.4.4 fundeps `LANGUAGE FunctionalDependencies`

Продолжение: как выглядит "волшебная пилюля" для пояснения компайлеру выводимых типов
в мульти-параметрических тайп-классах
```hs
-- при вызове
x *** y :: Double
-- компайлер нашел инстанс
instance Mult Int Double Double where
  i *** d = fromIntegral i * d

-- если же целевой тип не указать, поимеем ошибку
ghci> x *** y
<interactive>:15:1: error:     • No instance for (Mult Int Double ()) 
-- компайлер не стал догадываться и искать и выбирать варианты, просто послал нас нахер
-- при отсутствии точной спецификации

-- этой проблеме можно прописать таблетку: FunctionalDependencies
{-# LANGUAGE FunctionalDependencies #-}

class Mult a b c | a b -> c where
  (***) :: a -> b -> c
-- `| a b -> c`
-- описана зависимость, говорящая (в конечном итоге) что тип выхода определяется типами входа

ghci> x = 2 :: Int
ghci> y = pi :: Double
ghci> x *** y -- no problemo
6.283185307179586

-- вот на этом механизме и будем избавляться от ручного лифтинга в матрешках трансформеров

```
repl

### 4.4.5 test

```hs
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
class Functor' c e where
  fmap' :: (e -> e) -> c -> c

-- solution

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
class Functor' c e | c -> e where fmap' :: (e -> e) -> c -> c
instance (Eq a)=> Functor' [a] a where fmap' = fmap
instance (Num a, Eq a)=> Functor' (Maybe a) a where fmap' = fmap

-- alternatives

instance (Functor f)=> Functor' (f a) a where fmap' = fmap

instance Functor' [a] a where fmap' = fmap
instance Functor' (Maybe a) a where fmap' = fmap

```
test

### 4.4.6 рассуждения про интерфейсы

Так как же нам заменить `lift (tell ...)` на `tell ...` при работе с трансформерами?
Для начала, внесем нужные функции (tell) внутрь тайпкласса ...
Короче, идея простая: явно определить нужные интерфейсы для используемых монад.

Если внутри трансформера есть монада врайтер с интерфейсом `tell`, то и трансформер должен иметь этот интерфейс (API).

### 4.4.7 `MonadWriter w m | m -> w`

Начнем реализацию мультипараметрического тайп-класса,
выставляющего интерфейс монады `Writer` (пара `(a :: any, w :: Monoid)`)
```hs
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module WriterClass where
import MonadTrans -- (lift)
import StateT
...
import qualified WriterT -- содержимое этого модуля будет доступно только с префиксом WriterT.
-- подгрузили наши трансформеры из либ

-- создадим мульти-параметрический тайп-класс, описывающий интерфейс монады Writer
-- что (напоминаю) внутри кодируется как `(a, w)` где `w` это моноид лога
-- мы говорим, что инстанс этого тайп-класса, получивший на вход монаду эм и моноид даблю,
-- будет внутри иметь монаду Writer и обладать ее интерфейсом
class (Monoid w, Monad m) => MonadWriter w m | m -> w where -- лог: моноид, эм: монада
-- `| m -> w`: тип лога выводится из типа монады, т.е.
-- этот конструктор типов берет два параметра: тип монады и тип лога и,
-- при указании типа монады выводит тип лога
-- другими словами, мы абстрагируемся от устройства монады, гарантируя, что она будет Writer

  tell :: w -> m () -- tell :: Monad m => w -> WriterT w m ()
-- ранее это была свободная функция, получала тип лога и возвращала матрешку (некую абстрактную монаду внутри врайтера)
-- теперь это функция тайпкласса, которая запихивает лог в нашу (абстрактную) монаду, которая воспроизводит Writer,
-- наблюдаем абстрагирование от деталей устройства монады (трансформера) в пользу соответствия интерфейсу

  writer :: (a, w) -> m a -- -- writer :: (Monad m) => (a, w) -> WriterT w m a
  listen :: m a -> m (a, w) -- listen :: Monad m => WriterT w m a -> WriterT w m (a, w)

-- вспомогательная свободная функция, расширяет `listen` возможностью трансформировать лог через данную функцию
listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
-- берет преобразователь лога, монаду-с-интерфейсом-врайтера, возвращает монаду-врайтер где в качестве лога лежит
-- преобразованный лог из источника
listens f m = do
  ~(a, w) <- listen m
  return (a, f w)

```
repl

### 4.4.8 instance MonadWriter: (WriterT, StateT)

Займемся реализацией инстансов.
Наша задача: научить выставлять наружу интерфейс врайтера,
для всех трансформеров у которых внутри врайтер
```hs
import qualified WriterT as W -- мы уже реализовали либу

instance (Monoid w, Monad m) => MonadWriter w (W.WriterT w m) where
-- `MonadWriter w (W.WriterT w m)`: описание типа монады для которой реализуется инстанс тайпкласса,
-- это трансформер врайтера над логом `w` и произвольной монадой `m`
  writer = W.writer
  tell = W.tell
  listen = W.listen

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
-- `MonadWriter w (StateT s m)`: тип монады, реализующей интерфейс врайтера это трансформер стейт, внутри которого
-- произвольная монада но только если она врайтер, что фиксируется требованием `(MonadWriter w m)`
  writer = lift . writer
  tell = lift . tell
  listen m = StateT $ \s -> do -- мы знаем, что мы в трансформере стейт и можем его разворачивать,
  -- мы знаем, что внутренняя монада дает нам интерфейс врайтера, воспользуемся этим
    ~((a, s2), w) <- listen (runStateT m s) -- тут ничего интересного, просто перекладывание значений по парам,
    -- согласно сигнатурам. Плюс, ленивость пат.мат.
    return ((a, w), s2)

```
repl

### 4.4.9 пример подключения mtl vs transformers

Остались вопросы дизайна:
в Хаскелл есть либа transformers и есть либа MTL.
У них разные возможности совместимости, с разными версиями языка (были сделаны в разное время).
МТЛ не доступно без расширений языка.
```hs
-- в стандартной либе есть два варианта
-- https://hoogle.haskell.org/?hoogle=Writer&scope=set%3Ahaskell-platform
-- module Control.Monad.Trans.Writer -- transformers
-- module Control.Monad.Writer -- mtl (с мультипараметрическим тайп-классом)

-- у нас есть модуль WriterT, где лифт используется явно, нет мультипараметрических тайп-классов
-- и есть модуль WriterClass, где есть мультипараметрический тайп-класс врайтера
-- и мы напишем модуль Writer где будет как-бы mtl версия врайтера

module Writer ( -- export
  MonadWriter(..),
  listens,
  WriterT(WriterT),
  runWriterT,
  execWriterT
) where
import MonadTrans -- lift
import WriterClass
import WriterT (WriterT(WriterT), runWriterT, execWriterT)

-- после этого мы в пользовательском коде можем:
-- либо работать напрямую с WriterT, подключив его себе `import WriterT (...)`
-- либо работать с тайп-классоам Writer, подключив его себе `import Writer ...`

-- example

module Demo where
import Writer
import StateT
import Control.Monad.Identity

type SiWs = StateT Integer (WriterT String Identity)
test :: SiWs ()
test = do
  x <- get
  when (x > 100) (tell "Overflow") -- n.b. без лифта
  put 42

-- вот причина, по которой нам доступен tell без лифта:
-- instance (Monoid w, Monad m) => MonadWriter w (W.WriterT w m) where ...

runSiWs :: SiWs a -> Integer -> ((a, Integer), String)
runSiWs m = runIdentity . runWriterT . (runStateT m)

ghci> runSiWs test 133
(((),42),"Overflow") -- в лог попала запись, телл работает

```
repl

### 4.4.10 test

```hs
https://stepik.org/lesson/38581/step/10?unit=20506
TODO
{--
В этой и следующих задачах мы продолжаем работу с трансформером LoggT разработанным на [первом уроке этой недели](#4.1.12):

data Logged a = Logged String a deriving (Eq,Show)
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
write2log :: Monad m => String -> LoggT m ()
type Logg = LoggT Identity
runLogg :: Logg a -> Logged a

Теперь мы хотим сделать этот трансформер mtl-совместимым.

Избавьтесь от необходимости ручного подъема операций вложенной монады `State`, 
сделав трансформер `LoggT`, 
примененный к монаде с интерфейсом `MonadState`, 
представителем этого (`MonadState`) класса типов:

instance MonadState s m => MonadState s (LoggT m) where
  get   = undefined
  put   = undefined
  state = undefined

logSt' :: LoggT (State Integer) Integer      
logSt' = do 
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100

Проверка:

GHCi> runState (runLoggT logSt') 2
(Logged "30" 300,42)
--}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State

instance MonadState s m => MonadState s (LoggT m) where
  get   = undefined
  put   = undefined
  state = undefined

-- solution
-- догадался скопировать в решение инстансы функтора, аппликатива, монады и функции lift из прошлых заданий.
-- и задача решилась очень просто.

```
test

```hs
https://stepik.org/lesson/38581/step/11?unit=20506
TODO
{--
Избавьтесь от необходимости ручного подъема операций вложенной монады `Reader`, 
сделав трансформер `LoggT`, примененный к монаде с интерфейсом `MonadReader`, 
представителем этого (`MonadReader`) класса типов:

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = undefined
  local  = undefined
  reader = undefined

Для упрощения реализации функции `local` имеет смысл использовать вспомогательную функцию, 
поднимающую стрелку между двумя «внутренними представлениями» трансформера `LoggT` 
в стрелку между двумя `LoggT`:

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = undefined

Тест:

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
  Just x <- asks $ lookup 2                      -- no lift!
  write2log x
  Just y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
  write2log y

Ожидаемый результат:

GHCi> runReader (runLoggT logRdr) [(1,"John"),(2,"Jane")]
Logged "JaneJim" ()
--}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.Reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = undefined

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = undefined
  local  = undefined
  reader = undefined

-- solution
--  как mapLoggT использовать (ее, кстати, тесты не проверяют, оказалось  можно было и undefined оставить), 
-- но у людей, кто сообразил (в решениях) очень лаконично получилось

```
test

```hs
https://stepik.org/lesson/38581/step/12?unit=20506
TODO
{--
Чтобы избавится от необходимости ручного подъема операции `write2log`, 
обеспечивающей стандартный интерфейс вложенного трансформера `LoggT`, 
можно поступить по аналогии с другими трансформерами библиотеки mtl. 
А именно, разработать класс типов `MonadLogg`, выставляющий этот стандартный интерфейс

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

(Замечание: Мы переименовываем функцию `write2log` в `w2log`, 
поскольку хотим держать всю реализацию в одном файле исходного кода. 
При следовании принятой в библиотеках transformers/mtl идеологии они имели бы одно и то же имя, 
но были бы определены в разных модулях. При работе с transformers мы импортировали бы свободную функцию 
c квалифицированным именем `Control.Monad.Trans.Logg.write2log`, 
а при использовании mtl работали бы с методом класса типов `MonadLogg` с полным именем` Control.Monad.Logg.write2log`. )

Этот интерфейс, во-первых, должен выставлять сам трансформер `LoggT`, обернутый вокруг произвольной монады:

instance Monad m => MonadLogg (LoggT m) where
  w2log = undefined
  logg  = undefined

Реализуйте этого представителя, для проверки используйте:

logSt'' :: LoggT (State Integer) Integer      
logSt'' = do 
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

Результат должен быть таким:

GHCi> runState (runLoggT logSt'') 2
(Logged "BEGIN 30 END" 300,42)

Во-вторых, интерфейс `MonadLogg` должен выставлять любой стандартный трансформер, 
обернутый вокруг монады, выставляющей этот интерфейс:

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = undefined
  logg  = undefined

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = undefined
  logg  = undefined
  
-- etc...

Реализуйте двух этих представителей, для проверки используйте:

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer      
rdrStLog = do 
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

Результат должен быть таким:

GHCi> runLogg $ runStateT (runReaderT rdrStLog 4) 2
Logged "BEGIN 70 END" (700,42)
--}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = undefined
  logg  = undefined

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = undefined
  logg  = undefined

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = undefined
  logg  = undefined

-- solution

```
test

## chapter 4.5, Задачи на трансформеры

https://stepik.org/lesson/45331/step/1?unit=23645

В этом уроке вы выполните несколько заданий с помощью знаний, которые получили на этой неделе.

Теперь, когда мы уже хорошо понимаем принципы внутреннего устройства transformers и mtl, 
предполагается, что во всех последующих задачах вы будете использовать не наши самодельные трансформеры, 
а библиотечные. Не забывайте импортировать необходимые модули и подключать расширения языка.

```hs
https://stepik.org/lesson/45331/step/2?unit=23645
TODO
{--
Функция `tryRead` обладает единственным эффектом: 
в случае ошибки она должна прерывать вычисление. 
Это значит, что её можно использовать в любой монаде, предоставляющей возможность завершать вычисление с ошибкой, 
но сейчас это не так, поскольку её тип это делать не позволяет:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a

Измените её так, чтобы она работала в любой монаде, позволяющей сообщать об исключительных ситуациях типа `ReadError`. 
Для этого к трансформеру `ExceptT` в библиотеке `mtl` прилагается класс типов `MonadError` 
(обратите внимание на название класса — это так сделали специально, чтобы всех запутать), 
находящийся в модуле `Control.Monad.Except`
--}
import Control.Monad.Except

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/45331/step/3?unit=23645
TODO
{--
В очередной раз, у вас есть дерево строковых представлений чисел:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

и функция `tryRead`:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, MonadError ReadError m) => String -> m a

Просуммируйте числа в дереве, а если хотя бы одно прочитать не удалось, верните ошибку:

GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
Left (NoParse "oops")
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
Right 34

Подумайте, что общего у этой задачи с похожей, которую вы решали ранее (4.3.13)? 
В чем отличия? 
При чем здесь библиотека mtl?
--}
treeSum :: Tree String -> Either ReadError Integer
treeSum = undefined

-- solution

```
test

```hs
https://stepik.org/lesson/45331/step/4?unit=23645
TODO
{--
Вам дан список вычислений с состоянием (`State s a`) и начальное состояние. 
Требуется выполнить все эти вычисления по очереди (очередное вычисление получает на вход состояние, оставшееся от предыдущего) 
и вернуть список результатов. 
Но это ещё не всё. 
Ещё вам дан предикат, определяющий, разрешено некоторое состояние или нет; 
после выполнения очередного вычисления вы должны с помощью этого предиката проверить текущее состояние, 
и, если оно не разрешено, завершить вычисление, указав номер вычисления, которое его испортило.

При этом, завершаясь с ошибкой, мы можем как сохранить накопленное до текущего момента состояние, так и выкинуть его. 
В первом случае наша функция будет иметь такой тип:

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)

Во втором — такой:

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)

Пример:
— после выполнения вычисления с индексом 2 (нумерация с нуля) в состоянии оказалась тройка, что плохо:

GHCi> runLimited1 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
(Left 2,3)
GHCi> runLimited2 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
Left 2

Другой пример:
— всё хорошо, предикат всё время выполнялся. Вычисления ничего не возвращали (имели тип `State Int ()`), 
так что списки получились такие неинтересные:

GHCi> runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
(Right [(),(),(),()],4)
GHCi> runLimited2 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
Right ([(),(),(),()],4)

Если, прочитав про список, с каждым элементом которого надо совершить какое-то действие, вы сразу же подумали про traverse, 
то правильно сделали. Вопрос только в том, какое именно действие надо совершить с каждым элементом списка, 
но тут тоже всё довольно просто: надо выполнить вычисление с состоянием, описываемое элементом списка, 
а затем проверить состояние и, в случае проблемы, кинуть исключение:
(Прежде чем проходить по списку, мы ещё пронумеровали его элементы.)

limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

Собственно, остался сущий пустяк — запустить наше новое вычисление.

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

А теперь задание. Реализуйте функции `run1` и `run2`
--}
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable


run1 = undefined
run2 = undefined

-- solution
-- Ответ в этой книге "What I Wish I Knew When Learning Haskell "
-- очень помогла сигнатура
-- limited :: (MonadState a m, MonadError e m, Num e, Enum e) => (a -> Bool) -> [State a b] -> m [b]
-- И еще я включал расширения TypeApplications и PartialTypeSignatures, потому что run1 и run2 неоднозначно задают монаду m

```
test

```hs
https://stepik.org/lesson/45331/step/5?unit=23645
TODO
{--
Почти у каждого трансформера монад есть соответствующий ему класс типов 
(`StateT` — `MonadState`, `ReaderT` — `MonadReader`), 
хотя иногда имя класса построено иначе, нежели имя трансформера (`ExceptT` — `MonadError`).

Как называется класс, соответствующий трансформеру `MaybeT`? (Вопрос с небольшим подвохом.)
--}

-- solution
-- в качестве ответа требуется имя класса типов
-- семантика совершенно противоположенна "оборвать вычисления без дополнительной информации"

MonadPlus

> There is no class required for `MaybeT`, however. It is described entirely by `MonadPlus`﻿

```
test

```hs
https://stepik.org/lesson/45331/step/6?unit=23645
TODO
{--
Чтобы закончить наш курс ярко, предлагаем вам с помощью этой задачи в полной мере почувствовать на себе всю мощь continuation-passing style. 
Чтобы успешно решить эту задачу, вам нужно хорошо понимать, как работает CPS и монада ContT 
(а этого, как известно, никто не понимает). Кстати, это была подсказка.

Сопрограмма (корутина, coroutine) это обобщение понятия подпрограммы (по-простому говоря, функции). 
У функции, в отличие от сопрограммы, есть одна точка входа (то, откуда она начинает работать), 
а точек выхода может быть несколько, но выйти через них функция может только один раз за время работы; 
у сопрограммы же точек входа и выхода может быть несколько. Проще всего объяснить на примере:

coroutine1 = do
  tell "1"
  yield
  tell "2"

coroutine2 = do
  tell "a"
  yield
  tell "b"

GHCi> execWriter (runCoroutines coroutine1 coroutine2)
"1a2b"

Здесь используется специальное действие `yield`, 
которое передает управление другой сопрограмме. 
Когда другая сопрограмма возвращает управление (с помощью того же `yield` или завершившись), 
первая сопрограмма продолжает работу с того места, на котором остановилась в прошлый раз.

В общем случае, одновременно могут исполняться несколько сопрограмм, 
причем при передаче управления, они могут обмениваться значениями. 
В этой задаче достаточно реализовать сопрограммы в упрощенном виде: 
одновременно работают ровно две сопрограммы и значениями обмениваться они не могут.

Реализуйте трансформер `CoroutineT`, функцию `yield` для передачи управления и функцию `runCoroutines` для запуска. 
Учтите, что одна сопрограмма может завершиться раньше другой; другая должна при этом продолжить работу:

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

GHCi> execWriter (runCoroutines coroutine3 coroutine4)
"1ab2cd"
--}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- Пожалуйста, не удаляйте эти импорты. Они нужны для тестирующей системы.
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

newtype CoroutineT m a = CoroutineT { runCoroutineT :: ?? }

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines = undefined

yield :: Monad m => CoroutineT m ()
yield = undefined

-- solution
https://hoogle.haskell.org/?hoogle=Coroutine&scope=set%3Astackage

- Coroutine Pipelines by Mario Blaˇzevi´c https://themonadreader.files.wordpress.com/2011/10/issue19.pdf
- Part 1: Pause and resume https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coroutines-for-streaming/part-1-pause-and-resume

- https://hackage.haskell.org/package/monad-coroutine
лишний параметр-обертку можно просто убрать, чуть подправив реализации функтора и монады. 
Yield тоже сокращаем, убирая все навороты. 
И для runCoroutines ничего лишнего не нужно, я лишь выделил в отдельную функцию 
вариант довыполнения оставшейся корутины (тут смотрим на bounce и pogoStick).
И да, в реализации MonadWriter listen и pass не нужны, можно не мучаться


-- Вот код, который может помочь разобраться с ContT
ccc :: ContT () IO ()
ccc = do
  liftIO $ putStrLn "C"
  ContT $ \cont -> runContT ggg (cont)
  liftIO $ putStrLn "C"
  liftIO $ putStrLn "C"
  liftIO $ putStrLn "C"

ggg :: ContT () IO ()
ggg = do
  liftIO $ putStrLn "G"
  liftIO $ putStrLn "G"
  ContT $ \cont -> runContT fff (cont)
  liftIO $ putStrLn "G"
  liftIO $ putStrLn "G"
  
fff :: ContT () IO ()
fff = do
  liftIO $ putStrLn "F"
  liftIO $ putStrLn "F"
  liftIO $ putStrLn "F"
  liftIO $ putStrLn "F"
-- в середине вычисления ccc заменяем его продолжение на вычисление ggg, 
-- а чтобы продолжение ccc не пропало, отправляем его в конец вычисления ggg, аналогичное повторяем с fff
-- Такие действия являются аналогами yield, но им кое-чего не хватает (yield)

```
test




grep `TODO` markers, fix it.
