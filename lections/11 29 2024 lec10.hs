module Main where
-- Рекурсивные типы


--data [a]  = [] | a :[a]

data List a = Nil | Cons a (List a) -- Null | Node a (List a)
  deriving Show

-- Node 3 (Node 4 (Node 5 Null))
-- Cons 3 (Cons 4 (Cons 5 Nil))

-- data Nat = Zero | Succ Nat


-- Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, что существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.
--data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList = undefined

toList :: [a] -> List a
toList = undefined

--Рассмотрим еще один пример рекурсивного типа данных:

data Nat = Zero | Suc Nat

--Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее. Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.

--Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

--Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.

add :: Nat -> Nat -> Nat
add = undefined

mul :: Nat -> Nat -> Nat
mul = undefined

fac :: Nat -> Nat
fac = undefined

--Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node a (Tree a) (Tree a)

--Реализуйте функцию height, возвращающую высоту дерева, и функцию size, возвращающую количество узлов в дереве (и внутренних, и листьев). Считается, что дерево, состоящее из одного листа, имеет высоту 0.

height :: Tree a -> Int
height = undefined

size :: Tree a -> Int
size = undefined

--Теперь нам нужно написать функцию avg, которая считает среднее арифметическое всех значений в дереве. И мы хотим, чтобы эта функция осуществляла только один проход по дереву. Это можно сделать при помощи вспомогательной функции, возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go = undefined



main :: IO ()
main = putStrLn "Hello, world!"