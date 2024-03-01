{-# LANGUAGE InstanceSigs #-} -- позволяет писать сигнатуры для инстансов
-- {-# LANGUAGE TypeOperators #-} -- для разрешения `|.|` в качестве имени оператора над типами
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Using <$> on tuple" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Test_1_3_5 where

import Prelude (
    show, read, String, Char, Functor, fmap, Bool, otherwise, Int,
    (==), (*), (^), (-), (/), (++), id, const, Maybe (..), null, ($), succ, (.), undefined, Num ((+)),
    length, take, tail, zipWith, Fractional,
    zip, lookup, flip, Either(..), map, Foldable, return, fst, snd
    )

import Text.Parsec -- ( Parsec(..), getParserState, parseTest, many, many1 )
import Text.Parsec.Char -- ( alphaNum, string, string', digit, )

import Data.Char -- ( Char, digitToInt, isDigit, isLower )

import Data.Monoid -- ( Sum(..) )

import Control.Applicative (
    Applicative((<*>), pure, (<*)), (<$>), (<**>),
    Alternative((<|>), empty),
    ZipList(ZipList), getZipList,
    liftA, liftA2, (*>), (<*)
    )

import GHC.Show ( Show(..) )
import GHC.Base ( Eq(..) )

{--
Реализуйте парсер `getList`, который разбирает строки из чисел, разделенных точкой с запятой,
и возвращает список строк, представляющих собой эти числа:

GHCi> parseTest getList "1;234;56"
["1","234","56"]

GHCi> parseTest getList "1;234;56;"
parse error at (line 1, column 10):
unexpected end of input
expecting digit

GHCi> parseTest getList "1;;234;56"
parse error at (line 1, column 3):
unexpected ";"
expecting digit

Совет: изучите парсер-комбинаторы, доступные в модуле `Text.Parsec`, и постарайтесь найти наиболее компактное решение.
--}
-- import Text.Parsec
number :: Parsec String u String
number = many1 digit

sepNumber :: Parsec String u String
sepNumber = (char ';') *> many1 digit

getList :: Parsec String u [String]
getList = (:) <$> number <*> many sepNumber
-- getList = let elem = many1 digit <* optional (char ';') in many1 elem
-- getList = (many1 digit) `sepBy` (char ',')

{--
getList = do
    number <- many1 digit
    semicolon <- char ';'
    rest <- getList
    return (number:rest)
--}
-- если в какой-то момент при парсинге встречается ';', то потом обязано стоять число

test20 = parseTest getList "1;234;56" -- ["1","234","56"]
test21 = parseTest getList "1;234;56;" -- parse error at (line 1, column 10): unexpected end of input expecting digit
test22 = parseTest getList "1;;234;56" -- parse error at (line 1, column 3): unexpected ";" expecting digit


{--
Используя аппликативный интерфейс `Parsec`, реализуйте функцию `ignoreBraces`
которая принимает три аргумента-парсера. 
Первый парсер разбирает текст, интерпретируемый как открывающая скобка, 
второй — как закрывающая, 
а третий разбирает весь входной поток, расположенный между этими скобками
Возвращаемый парсер возвращает результат работы третьего парсера, скобки игнорируются

GHCi> test = ignoreBraces (string "[[") (string "]]") (many1 letter)
GHCi> parseTest test "[[ABC]]DEF"
"ABC"
--}
-- import Text.Parsec
ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces leftB rightB content = leftB *> content <* rightB

test = ignoreBraces (string "[[") (string "]]") (many1 letter)
test23 = parseTest test "[[ABC]]DEF" -- "ABC"
