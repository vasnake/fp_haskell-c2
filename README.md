# FP Haskell

[Часть 1 / 2](https://github.com/vasnake/fp_haskell-c1)

Часть 2 / 2
[Функциональное программирование на языке Haskell (часть 2) / Денис Москвин / stepik](https://stepik.org/course/693/syllabus) /
https://stepik.org/693

- [Chapter 1, аппликативные функторы](./chapter1.md)
- [Chapter 2, управление эффектами](./chapter2.md)
- [Chapter 3, монады и эффекты](./chapter3.md)
- [Chapter 4, трансформеры монад](./chapter4.md)

grep `TODO` markers, fix it.

## short notes

> чистые функции только лишь производят вычисления, а функции с эффектами
производят вычисления И/ИЛИ делают что-то еще. И вот это "что-то еще" и называется эффектами

Надо учесть, что аппликативный функтор это вычисление с эффектами.
Цепочка (пайплайн) таких вычислений может накапливать эффекты, но, в отличие от монад, не меняет "структуру контейнера".
Два вычисления, образующих "пайплайн", в аппликативе не могут "взаимодействовать", а в монаде могут.
https://stackoverflow.com/questions/23342184/difference-between-monad-and-applicative-in-haskell

Стандартные тайп-классы и их законы:
The standard Haskell libraries feature a number of type classes with algebraic or category-theoretic underpinnings https://wiki.haskell.org/Typeclassopedia

Композиция функций как инструмент преобразования выражений:
```hs
-- Берём выражение из двух функций: traverse, toList
traverse2list f x = traverse f (toList x)
-- выражаем через x
traverse2list f x = (traverse f) (toList x)
traverse2list f x = (traverse f . toList) x
-- η  - преобразование 1
traverse2list f = traverse f . toList
-- выражаем через f
traverse2list f = (. toList) (traverse f)
traverse2list f = ((. toList) . traverse) f
-- η  - преобразование 2
traverse2list = (. toList) . traverse
```
composition

Пайплайн вычислений (с эффектами), направление пайплайна, Applicative vs Monad
```hs
Операторы "аппликации":

-- функция, затем аргумент:
($)     ::                      (a -> b) ->   a ->   b -- infixr 0 $
(<$>)   :: Functor     f  =>    (a -> b) -> f a -> f b -- infixl 4 <$>, fmap
(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b -- infixl 4 <*>
(=<<)   :: Monad       m  =>  (a -> m b) -> m a -> m b -- infixr 1 =<<

-- аргумент, затем функция (через flip, за одним исключением - Applicative):
(&)     ::                      a ->   (a -> b) ->   b -- infixl 1 &    -- Data.Function
(<&>)   :: Functor     f  =>  f a ->   (a -> b) -> f b -- infixl 1 <&>  -- Control.Lens.Operators
(<**>)  :: Applicative f  =>  f a -> f (a -> b) -> f b -- infixl 4 <**> -- Control.Applicative
(>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b -- infixl 1 >>=

Аппликативы и монады позволяют строить пайплайны вычислений с эффектами.

Applicative vs Monad:
В аппликативе структура вычислений более жестко фиксирована.
В монаде дозволена большая гибкость при вычислениях, можно влиять на структуру контекста (на эффекты).
Монада: значение предыдущего вычисления может влиять на структуру следующих вычислений

(>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b -- infixl 1 >>=
во время ее выполнения создается контекст,
как итог, пайплайн связывает как вычисления, так и измениня контекста,
что не работает для аппликатива

(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b -- infixl 4 <*>
Согласно сигнатуре, аппликатив не может менять структуру контекста в процессе выполнения функции,
контекст жестко определен вторым параметром.

пайплайн в аппликативе списка
ghci> (,) <$> [1 .. 3] <*> ['a' .. 'c']
[
    (1,'a'),(1,'b'),(1,'c'),
    (2,'a'),(2,'b'),(2,'c'),
    (3,'a'),(3,'b'),(3,'c')]

пайплайн в монаде списка, результат идентичен
ghci> do { a <- [1 .. 3]; b <- ['a' .. 'c']; return (a, b) }
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

смотрите: вложенный цикл мы поменяли, он начинается со значения взятого из внешнего цикла
в итоге поменялась структура - размер списка (влияем на эффект)
попробуйте сделать это в аппликативе :)
ghci> do { a <- [1 .. 3]; b <- [a .. 3]; return (a, b) }
[
    (1,1),(1,2),(1,3),
    (2,2),(2,3),
    (3,3)]

```
applicative vs monad

## links

- The standard Haskell libraries feature a number of type classes with algebraic or category-theoretic underpinnings https://wiki.haskell.org/Typeclassopedia
- Ninety-Nine Problems in Haskell https://stepik.org/course/101204/info
- Pronunciation https://wiki.haskell.org/Pronunciation
- hooogle https://hoogle.haskell.org/?hoogle=State&scope=set%3Ahaskell-platform
- в Haskell для любого типа, для которого определен и выполняется первый закон функтора, выполняется и второй https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
- Theorems for free / Philip Wadler https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf
- Real World Haskellby Bryan O'Sullivan, Don Stewart, and John Goerzen https://book.realworldhaskell.org/read/using-parsec.html
- SKIing with Y*, Iota and Ackermann http://www.type.sh/?p=161
- Monad vs Applicative https://stackoverflow.com/questions/23342184/difference-between-monad-and-applicative-in-haskell
- GeneralizedNewtypeDeriving https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving
- phantom type https://wiki.haskell.org/Phantom_type
- The Monad.Reader https://themonadreader.wordpress.com/
- Alternative and MonadPlus https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
- MonadPlus and Monoid serve different purposes https://stackoverflow.com/questions/10167879/distinction-between-typeclasses-monadplus-alternative-and-monoid
- Скандальная правда об обработке исключений в Haskell https://eax.me/haskell-exceptions/
- Haskell mini-patterns handbook https://kowainik.github.io/posts/haskell-mini-patterns
- The Continuation Monad https://www.haskellforall.com/2012/12/the-continuation-monad.html
- Монада ContT в картинках (Haskell) https://habr.com/ru/articles/149174/
- How and why does the Haskell Cont monad work? https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work
- Continuation passing style https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
- The Evolution of a Haskell Programmer https://willamette.edu/~fruehr/haskell/evolution.html
- ListT done right https://wiki.haskell.org/ListT_done_right
- Understanding Functional Dependencies via Constraint Handling Rules https://www.google.com/search?q=Understanding+Functional+Dependencies+via+Constraint+Handling+Rules
- Functional dependencies https://wiki.haskell.org/Functional_dependencies
- creating data structures like cyclic or doubly linked lists https://wiki.haskell.org/Tying_the_Knot
