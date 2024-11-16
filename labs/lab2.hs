-- 17 : 1, 2, 14

{--  1
Придумать свой класс типов, который содержит как минимум
две функции, одна из которых выражается через другие.
Написать реализацию этого класса типов для любых двух типов
данных, типы данных выбирать такие, чтобы их реализации
отличались (можно использовать свои собственные типы
данных).
--}

class ListBaseDataContainer c where
  isEmpty :: c a -> Bool
  size :: c a -> Int

instance ListBaseDataContainer [] where
    isEmpty [] = True
    isEmpty _  = False

    size [] = 0
    size (_:xs) = 1 + size xs


data Tree a = EmptyNode | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

instance ListBaseDataContainer Tree where
    isEmpty EmptyNode = True
    isEmpty _     = False

    size EmptyNode = 0
    size (Node _ left right) = 1 + size left + size right


{-- пример
let myList = [1, 2, 3]
isEmpty myList
size myList 

let containerTree = Node 1 (Node 2 EmptyNode EmptyNode) (Node 3 (Node 4 EmptyNode EmptyNode) EmptyNode)
isEmpty containerTree
size containerTree

--}

{--  2
Дано бинарное дерево, найти сумму всех листьев.
--}

sumLeaves :: Num a => Tree a -> a
sumLeaves EmptyNode = 0
sumLeaves (Node x EmptyNode EmptyNode) = x
sumLeaves (Node x left right) = sumLeaves left + sumLeaves right + x

{-- пример
let tree = Node 1 (Node 2 EmptyNode EmptyNode) (Node 3 (Node 4 EmptyNode EmptyNode) EmptyNode)
sumLeaves tree

--}

{--  14
Реализовать функцию, вычисляющую математическое
выражение, записанное в виде строки. Строка может содержать
операторы: +, -, *, /, ^ (возводить можно только в целую
положительную степень); функции: sin, cos (функция имеет
наивысший приоритет); целые числа и вещественные с любым
разделителем (на ваш выбор) и скобки. Строка может содержать
пробелы. Результатом может быть число, ошибка вычисления
(например, деление на 0), ошибка парсинга (когда строка
содержит некорректное выражение).
--}





{-- пример

--}

