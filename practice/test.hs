myFst (x, _) = x
mySnd (_, y) = y

mySndFst ((_, _), (x, _)) = x

fun2 = \ x y -> (\ y -> x + y) x

fun3 x y = (mySum, mySum^2)
    where mySum = x + y

fun4 x y = let mySum = x + y in
    (mySum, mySum^2)

-- [1, 2, 3], [], x:xs ~ [1, 2, 3] = 1:2:3:[]
-- [1, 2, 3], tail, все, кроме головы

myHead (x:xs) = x

fun5 x y = x + y

fun6 [] = error "Text, empty list"
fun6 [x] = x
fun6 (_:y:_) = y

myHeadHead (_:y:_) = y

listSize [] = 0
listSize (x:xs) = 1 + listSize xs


-- найти сумму эл списка
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- вернуть эл списка по индексу
getElementAt :: [a] -> Integer -> a
getElementAt [] _ = error "Index out of list"
getElementAt (x:xs) n
  | n < 0 || n >= listSize (x:xs) = error "Index out of list"
  | n == 0 = x
  | otherwise = getElementAt (xs) (n - 1)

-- факториал
fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n - 1)

fact2 :: Integer -> Integer
fact2 n
    | n == 0 = 1
    | otherwise = n * fact (n - 1)

-- дописать факториал итеративно~
--fact3 0 = 1
--fact3 n = helper n k
  --  where
    --    helper 0 k =
      --  helper n k = helper (n - 1) (n * k)

-- фибоначи
fib :: Integer -> Integer
fib n = if n == 2 || n == 1 then 1 else fib (n-1) + fib (n-2)

fib2 1 = 0
fib2 2 = 1
fib2 n = helper n 0 1
    where 
        helper 1 prev _ = prev
        helper n prev current = helper (n - 1) current (prev + current)


-- карирование разреление (fun x) y разделение аргументов
-- сечение операторов
funDel del x = del x

-- Maybe: Just 5, Nothing

fun7 Nothing = "Nothing"
fun7 (Just x) = show x -- fun7 (Just 4)

-- Either: Left x, Right y

-- $ понижает приоритет вызора функции до 0, например чтобы сначала высчитывались аргументы, а после вызывалась функция
-- посмотреть приоритет :info fun7
-- helper n k = helper (n - 1) (n * k) ===== helper n k = helper (n - 1) $ n * k

-- написать map

myMap :: (Integer -> Integer) -> [Integer] -> [Integer]
myMap _ [] = []
myMap m (x:xs) = m x : myMap m xs 

myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter _ [] = []
myFilter m (x:xs) = if m x 
                    then x : myFilter m xs 
                    else myFilter m xs


-- Лабораторная 1. Ряд Вариант 17 нужно 2 варианта явное исп рек / неявное исп рекурсии
-- финкция sort

mySort :: Ord a => [a] -> [a]
mySort lst = bSort lst (length lst)
  where
    bSort xs n
      | n <= 1 = xs
      | otherwise = bSort (pass xs) (n - 1)
    
    pass (x:y:xs)
      | x > y     = y : pass (x:xs)
      | otherwise = x : pass (y:xs)
    pass [x] = [x]
    pass [] = []

-- [1..10] [1,3..10] [10,9..1] 
-- [x^2 | x <- [1..10]] [x^2 | x <- [1..10], x^2 > 10]

-- [x^2 + y | x <- [1..10], y <- [1..3]] -- 2, 3....
-- take 5 [1..10]

fun8 f1 f2 l@(_:xs)
  | mod (length l) 2 == 0 = map f1 l
  | otherwise = map f2 xs


-- sum . map (^2) $ [1..10]
-- ( sum . map (^2) ) [1..10]

union :: [a] -> [a] -> [a]
union [] y = y
union (x:xs) y = x : (union xs y)

-- list 1-10 вознести в 2, составить список, элементов которой сначала сумма элементов списка 3 эл минимальное 4 ср знач

-- someFun [sum, max, min] fun x = [sum y, max y, min y]
-- where y = fun x

-- someFun ops f = map (. map f) ops
-- map (\ f -> f [1..10]) (someFun [maximum, minimum] (^2))

someFun ops f x = map (\op -> op (map f x)) ops
-- someFun [sum, maximum, minimum] (^2) [1..10]

combineFunctions funcs = foldr (\f acc -> \x -> acc (f x)) id funcs
applyToList f xs = map f xs
-- applyToList ( combineFunctions [(+1), (^2)] ) [1..10]
