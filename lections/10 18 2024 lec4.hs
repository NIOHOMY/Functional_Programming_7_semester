module Main where

import Prelude hiding (filter, takeWhile, dropWhile, span, break, map, concat, concatMap, and, all, or, any, zipWith, zipWith3, foldr)
import Data.Char

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs') -- локальный псевдоним
  | p x       = dropWhile p xs'
  | otherwise = xs


span :: (a -> Bool) -> [a] -> ([a],[a])
span p xs = (takeWhile p xs, dropWhile p xs)

break :: (a -> Bool) -> [a] -> ([a],[a])
break p = span (not . p)


--Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
--Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.

--GHCi> readDigits "365ads"
--("365","ads")
--GHCi> readDigits "365"
--("365","")

readDigits :: String -> (String, String)
--readDigits [] = ("", "")
--readDigits xs =  span (isDigit) xs
--  where isDigit x | x >= '0' && x <= '9' = True
--                  | otherwise            = False
readDigits = span isDigit  
 
--Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

--GHCi> filterDisj (< 10) odd [7,8,10,11,12]
--[7,8,11]

filterDisj' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj' _ _ [] = []
filterDisj' p q (x:xs)
                  | p x || q x = x : filterDisj' p q xs
                  | otherwise = filterDisj' p q xs
--filterDisj p1 p2 = filter (\x -> p1 x || p2 x)


--Напишите реализацию функции qsort. Функция qsort должна принимать на вход список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара: для какого-то элемента x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x, и потом запускаться рекурсивно на обеих частях.
--GHCi> qsort [1,3,2,5]
--[1,2,3,5]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>=x) xs) 

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f



--Напишите функцию squares'n'cubes, принимающую список чисел, и возвращающую список квадратов и кубов элементов исходного списка. 
-- [2,3,4]
--[4,8,9,27,16,64]




squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

--Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке. Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.

perms :: [a] -> [[a]]
perms []     = [[]]
perms [x, y] = [[y,x]]
--perms (x:xs) = concatMap $ map (x:) (perms xs)  

and, or :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or [] = False
or (x:xs) = x || or xs

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- к каждому слову текста применить операцию reverse
-- words разбивает на слова по пробелам
-- unwords обратно склеивает слова в строку


revWords :: String -> String
revWords = unwords . map reverse . words 

--Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
--GHCi> delAllUpper "Abc IS not ABC"
--"Abc not" --data.Char.isUpper

delAllUpper :: String -> String
--delAllUpper = unwords . concatMap allUp . words where
--  allUp :: [Char] -> [[Char]]
--  allUp xs | all isUpper xs = []
--           | otherwise = [xs]
  
delAllUpper = unwords . filter (any isLower) . words

--delAllUpper = unwords . filter (not . all isUpper) . words


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _   []     _    = []
zipWith _   _      []   = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
-- (+) (,)

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = []
--Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины, содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.




max3 :: Ord a => [a] -> [a] -> [a] -> [a]
--max3 as bs = zipWith max (zipWith max as bs)
max3 = zipWith3 ((max .) . max)

-- [2,3,4] [5,6,7]
-- [2+5, 3*6, 4+7] = [7,18,11]






sumList :: [Integer] -> Integer
--sumList [] = 0
--sumList (x:xs) = x + sumList xs
sumList = foldr (+) 0

productList :: [Integer] -> Integer
productList [] = 1
productList (x:xs) = x * productList xs

concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs
--concatList = foldr (++) []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

--sumList = foldr (+) 0

--посчитать сумму квадратов положительных элеиментов списка
sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares = foldr (\x s -> if x > 0 then x^2 + s else s) 0 

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

and''', or''' :: [Bool] -> Bool
and''' [] = True
and'''(x:xs) = x && and''' xs

or''' [] = False
or''' (x:xs) = x || or''' xs

and', or' :: [Bool] -> Bool
and' = foldr (&&) True
or'  = foldr (||) False

-- foldr (-) 5 [1,2,3]
-- (1 - (2 - (3 - 5)))

{-

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

foldr f ini 1:2:3:[]
~> 1 `f` foldr f ini (2:3:[])
~> 1 `f` (2 `f` foldr f ini (3:[]))
~> 1 `f` (2 `f` (3 `f` foldr f ini ([])))
~> 1 `f` (2 `f` (3 `f` ini))
-}




main = do
  putStrLn "Hello, World!"