import Text.Read (readMaybe)
import Data.Char (isSpace)
import Control.Monad (guard)


{--
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Удаление ведущих пробелов
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- Парсер для значения (узла) типа a
parseValue :: Read a => String -> Maybe (a, String)
parseValue str =
    let (valueStr, rest) = span (not . isSpace) str
    in readMaybe valueStr >>= \val -> Just (val, skipSpaces rest)

-- Парсер для пустого дерева (Empty)
parseEmpty :: String -> Maybe (Tree a, String)
parseEmpty str
    | "Empty" `elem` words str = Just (Empty, drop (length "Empty") str)
    | otherwise = Nothing

-- Парсер для узла дерева: Node x left right
parseNode :: Read a => String -> Maybe (Tree a, String)
parseNode ('N':'o':'d':'e':' ':rest) = do
    -- Парсим значение узла
    (value, rest1) <- parseValue (skipSpaces rest)
    -- Парсим левое поддерево
    (left, rest2) <- parseSubtree rest1
    -- Парсим правое поддерево
    (right, rest3) <- parseSubtree rest2
    return (Node value left right, rest3)
parseNode _ = Nothing

-- Парсер для поддерева, которое может быть пустым или узлом
parseSubtree :: Read a => String -> Maybe (Tree a, String)
parseSubtree str
    | "Empty" `elem` words str = parseEmpty str
    | "Node" `elem` words str = parseNode str
    | "(" `elem` words str = parseTree str
    | otherwise = Nothing

-- Парсер для дерева: обработка скобок
parseTree :: Read a => String -> Maybe (Tree a, String)
parseTree ('(':rest) = do
    -- Пропускаем открывающую скобку
    let rest' = skipSpaces rest
    -- Парсим дерево
    (tree, rest'') <- parseNode rest'
    -- Пропускаем закрывающую скобку
    case skipSpaces rest'' of
        (')':rest''') -> Just (tree, rest''')
        _ -> Nothing
parseTree _ = Nothing

-- Функция для удаления листовых узлов
removeLeaves :: Tree a -> Tree a
removeLeaves Empty = Empty
removeLeaves (Node x Empty Empty) = Empty  -- Это лист, удаляем его
removeLeaves (Node x left right) = 
    Node x (removeLeaves left) (removeLeaves right)

-- Функция для вывода дерева
printTree :: Show a => Tree a -> IO ()
printTree Empty = putStrLn "Empty"
printTree (Node x left right) = do
    putStrLn $ "Node " ++ show x
    putStrLn "Left subtree:"
    printTree left
    putStrLn "Right subtree:"
    printTree right

-- Функция для удаления листовых узлов с вводом пользователя
removeLeavesIO :: IO ()
removeLeavesIO = do
    putStrLn "Enter the tree in the format Node x left right (for example: Node 1 Empty Empty):"
    input <- getLine
    let tree = readTree input
    case tree of
        Just t -> do
            let newTree = removeLeaves (t)
            putStrLn "Tree after removing leaf nodes:"
            printTree t
            printTree newTree
        Nothing -> putStrLn "Error while parsing the tree. Try again."
    menu

 
-- Главный парсер для дерева
readTree :: String -> Maybe (Tree Integer)
readTree input = fmap fst (parseTree (skipSpaces input))

-- Главное меню программы
menu :: IO ()
menu = do
    putStrLn "Select an action:"
    putStrLn "1. Remove leaf nodes from the tree"
    putStrLn "2. Exit"
    choice <- getLine
    case choice of
        "1" -> removeLeavesIO
        "2" -> putStrLn "Exiting the program."
        _   -> do
            putStrLn "Invalid choice. Try again."
            menu

-- Основная программа
main :: IO ()
main = do
    putStrLn "Welcome to the Binary Tree Program!"
    menu

--}

{--
1. Реализовать консольный интерфейс для 3-ей задачи из 2-ой
лабораторной работы, используя монаду IO и do нотацию.
Интерфейс должен давать возможность выполнить все
реализованные функции. Программа должна завершаться только
после того как пользователь выберет ответствующую опцию, то
есть должна быть возможность выполнить несколько функций
до завершения программы.

3. Дано бинарное дерево, необходимо удалить листовые элементы
(те которые были листовыми в исходном дереве).
Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
--}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

createTreeFromInput :: IO (Tree Int)
createTreeFromInput = do
    putStrLn "Tree root enter:"
    root <- readLn
    left <- createSubtree "left"
    right <- createSubtree "right"
    return (Node root left right)

-- Функция для создания поддерева (рекурсивно)
createSubtree :: String -> IO (Tree Int)
createSubtree side = do
    putStrLn $ "Is " ++ side ++ " subtree? (y/n)"
    choice <- getLine
    case choice of
        "y" -> do
            putStrLn $ "Enter value for " ++ side ++ " subtree:"
            value <- readLn
            left <- createSubtree "lest"
            right <- createSubtree "right" 
            return (Node value left right)
        "n" -> return Empty
        _   -> do
            putStrLn "Undef 'y' or 'n'."
            createSubtree side

removeLeaves :: Tree a -> Tree a
removeLeaves Empty = Empty
removeLeaves (Node _ Empty Empty) = Empty
removeLeaves (Node val left right) = Node val (removeLeaves left) (removeLeaves right)

treeToString :: Show a => Tree a -> String
treeToString Empty = "Empty"
treeToString (Node val left right) = "Node " ++ show val ++ " (" ++ treeToString left ++ ") (" ++ treeToString right ++ ")"

menu :: IO ()
menu = do
    putStrLn "Choose:"
    putStrLn "1. Create tree"
    putStrLn "2. Delete leaves"
    putStrLn "3. Print tree"
    putStrLn "4. Exit"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Delete leaves from tree..."
            tree <- createTreeFromInput 
            let treeWithoutLeaves = removeLeaves tree
            putStrLn ("Result: " ++ treeToString treeWithoutLeaves)
            menu
        "2" -> do
            putStrLn "Print inputed tree:"
            tree <- createTreeFromInput  -- Для теста создаем дерево заново
            putStrLn ("Tree: " ++ treeToString tree)
            menu
        "3" -> putStrLn "Exit."
        _   -> do
            putStrLn "undefine."
            menu

-- Основная функция
main :: IO ()
main = menu

{--
2. Используя монаду список и do нотацию, реализовать функцию,
которая будет возвращать все возможные комбинации
результатов подбрасывания монеты после n бросков, в которых
орел выпадал чаще чем решка. Подсказка: будет удобнее, если
реализовать следующие функции.
    2.1. nextCoinToss :: [Coin] -> [[Coin]] - функция принимает в
    качестве параметра список результатов предыдущих бросков.

    2.2. nextCoinTossN :: [Coin] -> Int -> [[Coin]] - функция
    принимает в качестве параметра список результатов
    предыдущих бросков, и число сколько еще нужно сделать
    бросков.
--}

data Coin = H | T | E
  deriving (Show, Eq)

nextCoinTossN :: [Coin] -> Int -> [[Coin]]
nextCoinTossN tosses 0 = [tosses]  -- нет бросков
nextCoinTossN tosses n = do
  toss <- [H, T, E]
  nextCoinTossN (tosses ++ [toss]) (n - 1)

validCombinations :: Int -> [[Coin]]
validCombinations n = do
  tosses <- nextCoinTossN [] n
  let heads = length (filter (== H) tosses)  -- орёл
      tails = length (filter (== T) tosses)  -- решка
  guard (heads > tails)
  return tosses

main2 :: IO ()
main2 = do
    putStrLn "Choose:"
    nStr <- getLine
    case readMaybe nStr of
        Just n -> do
            let result = validCombinations n
            print result
        Nothing -> putStrLn "Invalid input! Please enter a valid number."

