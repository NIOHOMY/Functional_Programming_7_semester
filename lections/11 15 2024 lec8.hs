module Main where

data Person' = Person' String String Int deriving (Show)

firstName':: Person' -> String
firstName' (Person' x _ _) = x

lastName':: Person' -> String
lastName' (Person' _ x _) = x

age':: Person' -> Int
age' (Person' _ _ x) = x -- age' john

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Show, Eq)

-- let john = Person "John" "Smith" 20
-- let jane = Person {age = 25, firstName = "Jane", lastName = "Doe"}
-- let unknownBill = Person {firstName = "Bill"}
-- age john

(&) :: a -> (a -> b) -> b
x & f = f x

-- john & firstName

{- Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:

    timestamp — время, когда произошло событие (типа UTCTime);
    logLevel — уровень события (типа LogLevel);
    message — сообщение об ошибке (типа String).

 Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, и функцию logEntryToString, возвращающую текстуальное представление записи в виде:
<время>: <уровень>: <сообщение>
-}

updateAge :: Int -> Person -> Person
updateAge newAge person = person {age = newAge}

-- updateAge 25 john --> 

--Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.





updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person1 {lastName = person2 & lastName} 

-- Сопоставление с образцом в функциях:

name :: Person -> String
name person = firstName person ++ " " ++  lastName person

name' :: Person -> String
name' (Person fn ln _) = fn ++ " " ++ ln

name'' :: Person -> String
name'' (Person {firstName = fn, lastName = ln}) = fn ++ " " ++ ln

--Допустим мы объявили тип
data Shape = Circle Double | Rectangle Double Double
--Что произойдет при объявлении такой функции:
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

--Она не компилируется, так как объявление типа Shape не использует синтаксис записей
--Она не компилируется из-за синтаксической ошибки
--Она компилируется и всегда возвращает True
--Она компилируется и всегда возвращает False
--Она компилируется и возвращает True, если на вход передается Rectangle, иначе она возвращает False 

-- Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", то после применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.

abbrFirstName :: Person -> Person




abbrFirstName p@(Person {firstName = (x:_:_)}) = p {firstName = [x, '.']}
abbrFirstName p = p







main :: IO ()
main = putStrLn "Hello, world!"