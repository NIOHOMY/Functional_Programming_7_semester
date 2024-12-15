import Text.Read (readMaybe)
import Data.Char (isSpace)
import Control.Monad (guard)


{--
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

parseValue :: Read a => String -> Maybe (a, String)
parseValue str =
    let (valueStr, rest) = span (not . isSpace) str
    in readMaybe valueStr >>= \val -> Just (val, skipSpaces rest)

parseEmpty :: String -> Maybe (Tree a, String)
parseEmpty str
    | "Empty" `elem` words str = Just (Empty, drop (length "Empty") str)
    | otherwise = Nothing

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

parseSubtree :: Read a => String -> Maybe (Tree a, String)
parseSubtree str
    | "Empty" `elem` words str = parseEmpty str
    | "Node" `elem` words str = parseNode str
    | "(" `elem` words str = parseTree str
    | otherwise = Nothing

parseTree :: Read a => String -> Maybe (Tree a, String)
parseTree ('(':rest) = do
    let rest' = skipSpaces rest
    (tree, rest'') <- parseNode rest'
    case skipSpaces rest'' of
        (')':rest''') -> Just (tree, rest''')
        _ -> Nothing
parseTree _ = Nothing

removeLeaves :: Tree a -> Tree a
removeLeaves Empty = Empty
removeLeaves (Node x Empty Empty) = Empty  -- Это лист, удаляем его
removeLeaves (Node x left right) = 
    Node x (removeLeaves left) (removeLeaves right)

printTree :: Show a => Tree a -> IO ()
printTree Empty = putStrLn "Empty"
printTree (Node x left right) = do
    putStrLn $ "Node " ++ show x
    putStrLn "Left subtree:"
    printTree left
    putStrLn "Right subtree:"
    printTree right

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

readTree :: String -> Maybe (Tree Integer)
readTree input = fmap fst (parseTree (skipSpaces input))

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
        
main :: IO ()
main = do
    putStrLn "Welcome."
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

createSubtree :: String -> IO (Tree Int)
createSubtree side = do
    putStrLn $ "Is " ++ side ++ " subtree? (y/n)"
    choice <- getLine
    case choice of
        "y" -> do
            putStrLn $ "Enter value for " ++ side ++ " subtree:"
            value <- readLn
            left <- createSubtree "left subtree"
            right <- createSubtree "right subtree" 
            return (Node value left right)
        "n" -> return Empty
        _   -> do
            putStrLn "Undefine: 'y' or 'n'."
            createSubtree side

removeLeaves :: Tree a -> Tree a
removeLeaves Empty = Empty
removeLeaves (Node _ Empty Empty) = Empty
removeLeaves (Node val left right) = Node val (removeLeaves left) (removeLeaves right)

treeToString :: Show a => Tree a -> String
treeToString Empty = "Empty"
treeToString (Node val left right) = "Node " ++ show val ++ " (" ++ treeToString left ++ ") (" ++ treeToString right ++ ")"

printTree :: Show a => Tree a -> IO ()
printTree Empty = putStrLn "Empty"
printTree (Node x left right) = do
    putStrLn $ "Node " ++ show x
    putStrLn "Left subtree:"
    printTree left
    putStrLn "Right subtree:"
    printTree right

menu :: IO ()
menu = do
    putStrLn "Choose:"
    putStrLn "1. Delete leaves"
    putStrLn "2. Print tree"
    putStrLn "3. Exit"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Delete leaves from tree..."
            tree <- createTreeFromInput 
            let treeWithoutLeaves = removeLeaves tree
            putStrLn ("Tree line view: " ++ treeToString tree)
            putStrLn "Tree lvl view: "
            printTree tree
            putStrLn ("Result line view: " ++ treeToString treeWithoutLeaves)
            putStrLn "Result lvl view: "
            printTree treeWithoutLeaves
            menu
        "2" -> do
            putStrLn "Print inputed tree:"
            tree <- createTreeFromInput  -- тест
            putStrLn ("Tree line view: " ++ treeToString tree)
            putStrLn "Tree lvl view: "
            printTree tree
            menu
        "3" -> putStrLn "Exit."
        _   -> do
            putStrLn "Undefine."
            menu

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

nextCoinToss :: [Coin] -> [[Coin]]
nextCoinToss tosses = do
  toss <- [H, T, E]
  return (tosses ++ [toss])

nextCoinTossN :: [Coin] -> Int -> [[Coin]]
nextCoinTossN tosses 0 = [tosses]
nextCoinTossN tosses n = nextCoinToss tosses >>= \toss -> nextCoinTossN toss (n - 1)

validCombinations :: Int -> [[Coin]]
validCombinations n = do
  tosses <- nextCoinTossN [] n
  let eagle = length (filter (== H) tosses)  -- количество орлов
      tails = length (filter (== T) tosses)  -- количество решек
      edges = length (filter (== E) tosses)  -- количество рёбер
  guard (eagle > tails) -- орёл чаще чем решка
  return tosses

-- Основная функция
main2 :: IO ()
main2 = do
    putStrLn "Enter the number of coin tosses:"
    nStr <- getLine
    case readMaybe nStr of
        Just n -> do
            let result = validCombinations n
            print result
        Nothing -> putStrLn "Error! Please enter a valid number."
