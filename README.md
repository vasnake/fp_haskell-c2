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

## links

- The standard Haskell libraries feature a number of type classes with algebraic or category-theoretic underpinnings https://wiki.haskell.org/Typeclassopedia#Introduction
- Ninety-Nine Problems in Haskell https://stepik.org/course/101204/info
- Pronunciation https://wiki.haskell.org/Pronunciation
- hooogle https://hoogle.haskell.org/?hoogle=State&scope=set%3Ahaskell-platform
- в Haskell для любого типа, для которого определен и выполняется первый закон функтора, выполняется и второй https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
- Theorems for free / Philip Wadler https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf
- Real World Haskellby Bryan O'Sullivan, Don Stewart, and John Goerzen https://book.realworldhaskell.org/read/using-parsec.html
- SKIing with Y*, Iota and Ackermann http://www.type.sh/?p=161
