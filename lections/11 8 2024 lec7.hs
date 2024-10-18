module Main where

data B = T | F deriving (Show, Eq, Read, Enum)

not' :: B -> B
not' T = F
not' F = T

-- succ T, T == F

-- Тип данных Color определен следующим образом



--Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.

--GHCi> show Red
--"Red"
data Color = Red | Green | Blue --deriving Show

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar 6 = '6'
intToChar 7 = '7'
intToChar 8 = '8'
intToChar 9 = '9'
intToChar _ = 'N'

isz :: Char -> Bool
isz 'z' = True
isz  _  = False


stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False
--stringToBool _ = error "Неизвестное значение"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

foo 1 2 = 3
foo 0 x = 5

--foo 0 undefined = 5
-- foo undefined 0 - Exception
-- foo 1 (5-3) = 3

bar (1, 2) = 3
bar (0, _) = 5

-- Пусть определены следующие функции:

emptyOrSingleton :: Bool -> Double -> [Double]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

--Выберите варианты вызовов этих функций, при которых сопоставление с образцом будет осуществлено успешно.

--isEqual (undefined, undefined) undefined
--isEqual undefined undefined
--isEqual (undefined, undefined) (undefined, undefined)
--emptyOrSingleton True undefined
--isEqual undefined (undefined, undefined)
--emptyOrSingleton False undefined
--emptyOrSingleton undefined 5 


-- Тип LogLevel описывает различные уровни логирования.

data LogLevel = Error | Warning | Info

--Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.

--GHCi> cmp Error Warning
--GT
--GHCi> cmp Info Warning
--LT
--GHCi> cmp Warning Warning
--EQ



cmp :: LogLevel -> LogLevel -> Ordering

cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp _ Info = GT
cmp Info _ = LT

lessThenError :: LogLevel -> Bool
lessThenError lvl = 
  case cmp lvl Error of  
    LT -> True
    _  -> False

{-- Пусть объявлен следующий тип данных:

data Result = Fail | Success

--И допустим определен некоторый тип данных SomeData и некоторая функция

--doSomeWork :: SomeData -> (Result,Int)

--возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.

--Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения, либо строку "Fail: N" в случае неудачи, где N — код ошибки.

--processData :: SomeData -> String
--processData = undefined
--}

data Point = Point Double Double --deriving (Show, Eq)

-- :t Pt ~> Double -> Double -> Point

origin :: Point
origin = Point 0.0 0.0

distToOrigin :: Point -> Double
distToOrigin (Point x y) = sqrt (x^2 + y^2)

--
data Roots = Roots Double Double | NoRoots
  deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c 
  | discr >= 0 = Roots x1 x2
  | otherwise = NoRoots
  where  
    x1 = helper (-d)
    x2 = helper d
    helper x = (-b + x) / (2 * a)
    d = sqrt discr
    discr = b^2 - 4 * a * c

--

--Определим тип фигур Shape:

data Shape = Circle Double | Rectangle Double Double

--У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b. Реализуйте функцию area, возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.
    
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b


-- Функция doSomeWork возвращает результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха. Такое определение функции не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки.

--Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код ошибки только в случае неудачи. Для этого необходимо определить тип Result'. Кроме того, определите instance Show для Result' так, чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.

data Result = Fail | Success

--doSomeWork :: SomeData -> (Result,Int)

--Порождение квадрата из прямоугольника
square :: Double -> Shape
square a = Rectangle a a

--Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False

--Целое число можно представить как список битов со знаком.

--Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в начале списка, а старшие — в конце. Можно считать, что на вход не будут подаваться числа с ведущими нулями. 

data Bit = Zero | One
--data Sign = Minus | Plus
data Z = Z [Bit]

add :: Z -> Z -> Z
add (Z [_]) (Z [_]) = undefined

mul :: Z -> Z -> Z
mul = undefined






main = putStrLn "Hello, world!"