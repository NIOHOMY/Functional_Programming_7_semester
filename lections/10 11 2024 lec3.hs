module Main where

import Prelude hiding (length, (++), null, last, init, reverse, zip,zip3, unzip)
-- создать пустой список
emptyList :: [Int]
emptyList = []

-- создать список из одного элемента
singletonList :: a -> [a]
singletonList x = x : []


--Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.
addTwoElements :: a -> a -> [a] -> [a]


addTwoElements a b xs = a : (b : xs)  

--Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. Количество повторов определяется значением второго аргумента этой функции.
nTimes:: a -> Int -> [a]




nTimes  a n | n == 0 = []
            | n > 0  = a : nTimes a (n-1)
            | otherwise = error "n must be >= 0"



-- head, tail 
-- второй элемент списка
second :: [a] -> a
second xs = head (tail xs)
--second - head . tail

--сопоставление с образцом
head' ((:) x xs) = x --head' ((:) x _) = x 
--(x : xs)
tail' (x : xs) = xs --tail' (_ : xs) = xs
--tail' [2,3,4,5] 2 : [3,4,5]
second' :: [a] -> a
second' (_ : xs) = head xs 

second'' :: [a] -> a
second'' (_ : x : _) = x 

--Исследуйте тип функции

sndHead = snd . head

--и разберитесь, каково ее поведение. Эту функцию можно реализовать, используя сопоставление с образцом

sndHead' ((_,x) : _) = x --[(2,4), (5,7), (8,9)]
       --((2,4) : [(5,7), (8,9)])
--Отметьте те образцы, которые подходят для этой цели.
--((,) ((:) _ _) x)  
--((,) y x : z)
--((,) y z : x)
--((:) ((,) _ x) y)
--((,) x y : z)
--((_, x) : _)



length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs





(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys  --(++) [] ys = ys
(x:xs) ++ ys = x : (xs ++ ys)





null :: [a] -> Bool
null [] = True
null _  = False


--Cформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.

--GHCi> oddsOnly [2,5,7,10,11,12]
--[5,7,11]

--Для анализа четности можно использовать функции odd и even стандартной библиотеки.

oddsOnly :: Integral a => [a] -> [a]




oddsOnly [] = []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs 

oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' [] = []
oddsOnly' (x:xs)
    | odd x     = x : oddsOnly' xs
    | otherwise = oddsOnly' xs

--последний элемент
last :: [a] -> a
--last [1,2,3]
--last (x:xs)
--     (1:[2,3])
last (x:[]) = x
last (_:xs) = last xs

--список без последнего элемента
init :: [a] -> [a]


init [x] = []
init (x:xs) = x : init xs

--
--sum :: (Num a) => [a] -> a
--maximum :: (Ord a) => [a] -> a

reverse :: [a] -> [a]
reverse lst = rev lst [] where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

--Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.

isPalindrome :: Eq a => [a] -> Bool
--isPalindrome []    = True
--isPalindrome [x]   = True
isPalindrome xs    = xs == reverse xs 

zip :: [a] -> [b] -> [(a,b)]
zip [] _  = []
zip as [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3   _      _      _    = []

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y):xys) = 
  let (xs, ys) = unzip xys
  in (x:xs, y:ys)

--Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.
  
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 []     []     []     = []
sum3 (x:xs) []     []     = x : sum3 xs [] []
sum3 []     (x:xs) []     = x : sum3 [] xs []
sum3 []     []     (x:xs) = x : sum3 [] [] xs
sum3 (x:xs) []     (y:ys) = (x+y) : sum3 xs [] ys
sum3 []     (x:xs) (y:ys) = (x+y) : sum3 [] xs ys
sum3 (x:xs) (y:ys) []     = (x+y) : sum3 xs ys []
sum3 (x:xs) []     (y:ys) = (x+y) : sum3 xs [] ys
sum3 []     (x:xs) (y:ys) = (x+y) : sum3 [] xs ys
sum3 (x:xs) (y:ys) (z:zs) = (x+y+z) : sum3 xs ys zs

{--
sum3 xs ys zs = sum3' xs ys zs where
  sum3' [] [] [] = []
  sum3' xs ys zs = (head' xs + head' ys + head' zs) : sum3' (tail' xs) (tail' ys) (tail' zs)

  head' [] = 0
  head' (x:_) = x

  tail' [] = []
  tail' (_:xs) = xs


sum3 a b = sum2 $ sum2 a b 

sum2 :: Num a => [a] -> [a] -> [a]
sum2 xs [] = xs
sum2 [] ys = ys
sum2 (x : xs) (y : ys) = x + y : sum2 xs ys
--}

--Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.
--GHCi> groupElems []
--[]
--GHCi> groupElems [1,2]
--[[1],[2]]
--GHCi> groupElems [1,2,2,2,4]
--[[1],[2,2,2],[4]]
--GHCi> groupElems [1,2,3,2,4]
--[[1],[2],[3],[2],[4]]

groupElems :: Eq a => [a] -> [[a]]
groupElems []  = []
groupElems [x] = [[x]]
groupElems x   = helper' (helper'' x)
  where helper'' [] = []
        helper'' (x:xs) = [x] : helper'' xs
        helper' :: Eq a => [[a]] -> [[a]]
        helper' [] = []
        helper' [x] = [x]
        helper' ((x:xs):[y]:xys) | x == y = helper' ((x:y:xs):xys) 
                                 | otherwise = (x:xs) : helper' ([y]:xys)
  
{--
groupElems (x : xs) | x == head r = (x : r) : rs
                  | otherwise   = [x] : r : rs                                                                                                                               
where (r : rs) = groupElems xs
--}