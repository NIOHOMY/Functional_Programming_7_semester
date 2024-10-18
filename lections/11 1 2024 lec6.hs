module Main where

import Prelude hiding (foldr1, foldl1, foldl, scanl, foldr, scanr, unfoldr)
--import Data.List (scanl)


foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ [] = error "foldr1: EmptyList"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "foldl1: EmptyList"

maximum :: (Ord a) => [a] -> a
maximum = foldl1 max

--Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.

lastElem :: [a] -> a

lastElem  = foldl1 (\_ y -> y )

--lastElem  = foldl1 $ const id

--lastElem  = foldl1 $ flip const

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ ini [] = ini
foldl f ini (x:xs) = foldl f (ini `f` x) xs

{-
foldl f ini [1,2,3] ~>> ((ini `f` 1) `f` 2) `f` 3

??? ~>> [ini, ini `f` 1, (ini `f` 1) `f` 2, ((ini `f` 1) `f` 2) `f` 3]
-}
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f ini [] = [ini]
scanl f ini (x:xs) = ini : scanl f (ini `f` x) xs

facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1..]

--facs !! 5 ~>> 120

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

--partialSums [1..10] ~>> [0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55] 

-- обратные факториалы: map (**(-1)) facs

-- обратные факториалы сходятся к экспоненте:
-- take 15 . partialSums . map (**(-1)) $ facs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

{-
foldr f ini [1,2,3] ~>> 1 `f` (2 `f` (3 `f` ini))

??? ~>> [1 `f` (2 `f` (3 `f` ini)), 2 `f` (3 `f` ini), 3 `f` ini, ini]
-}
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ ini [] = [ini]
scanr f ini (x:xs) = (x `f` q) : qs
  where qs@(q:_) = scanr f ini xs

-- scanr (+) 0 [] ~>> [0]
-- scanr (+) 0 [1,2,3] ~>> [6,5,3,0]

unfold :: (b -> (a, b)) -> b -> [a]
unfold f ini = let (x, ini') = f ini in
  x : unfold f ini'

{-
iterate f x == [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]

iterate f = unfold (\x -> (x,f x))
-}

-- find :: (a -> Bool) -> [a] -> Maybe a
-- find odd [2,8,4]

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- lookup 2 [(2,'d'), (4,'e')]

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where 
  helper (Just (x,ini')) = x : unfoldr f ini'
  helper Nothing         = []

-- unfoldr (\x -> if x==10 then Nothing else Just (x, x+2)) 0 

--Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b. 

--GHCi> revRange ('a','z')
--"zyxwvutsrqponmlkjihgfedcba"


revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = undefined

main :: IO ()
main = putStrLn "Hello, world!"