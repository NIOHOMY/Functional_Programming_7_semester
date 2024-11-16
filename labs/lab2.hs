import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (liftA2)
import Text.Parsec.Expr (buildExpressionParser, Operator(..))

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


data Expr
    = Number Double        -- Число
    | Add Expr Expr        -- Сложение
    | Subtract Expr Expr   -- Вычитание
    | Multiply Expr Expr   -- Умножение
    | Divide Expr Expr     -- Деление
    | Power Expr Expr      -- Возведение в степень
    deriving Show

-- числа
parseNumber :: Parser Expr
parseNumber = do
    n <- many1 (digit <|> char '.')
    return . Number $ read n

--  умножения и деления приор выше
parseMulDiv :: Parser Expr
parseMulDiv = parsePower `chainl1` (try (char '*' *> pure Multiply) <|> try (char '/' *> pure Divide))

parseAddSub :: Parser Expr
parseAddSub = parseMulDiv `chainl1` (try (char '+' *> pure Add) <|> try (char '-' *> pure Subtract))

-- приор выше чем умножение и деление
parsePower :: Parser Expr
parsePower = parseFactor `chainl1` (try (char '^' *> pure Power))

parseFactor :: Parser Expr
parseFactor = try parseNumber <|> (char '(' *> parseAddSub <* char ')')


parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> parseAddSub <* spaces) ""

eval :: Expr -> Either String Double
eval (Number n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Subtract a b) = (-) <$> eval a <*> eval b
eval (Multiply a b) = (*) <$> eval a <*> eval b
eval (Divide a b) = do
    divisor <- eval b
    if divisor == 0
        then Left "Ошибка: деление на ноль"
        else (/) <$> eval a <*> pure divisor
eval (Power a b) = do
    base <- eval a
    exp' <- eval b
    if exp' < 0 || exp' /= fromInteger (round exp')
        then Left "Ошибка: степень должна быть целым положительным числом"
        else Right (base ** exp')


calculate :: String -> Either String Double
calculate str = case parseExpr str of
    Left err -> Left ("Ошибка парсинга: " ++ show err)
    Right expr -> eval expr
