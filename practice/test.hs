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