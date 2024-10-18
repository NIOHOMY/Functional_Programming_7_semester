module Main where

data CoordD = CoordD Double Double

data CoordI = CoordI Int Int

-- Параметризованные типы:

data Coord a = Coord a a deriving Show

-- Coord (3::Int) (5::Int)

-- Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами, и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.

distance :: Coord Double -> Coord Double -> Double

distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

--Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат. Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты. Один из углов этой ячейки имеет координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.

--Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, и функцию getCell, которая принимает координату точки и возвращает номер ячейки в которой находится данная точка. В качестве первого аргумента обе эти функции принимают ширину ячейки.


getCenter :: Double -> Coord Int -> Coord Double

getCenter r (Coord x y) = Coord (fromIntegral x * r + 0.5 * r) (fromIntegral y * r + 0.5 * r)


getCell :: Double -> Coord Double -> Coord Int
getCell r (Coord x y) = Coord (floor (x / r)) (floor (y / r))


--Реализуйте функцию findDigitOrX, использующую функцию findDigit. findDigitOrX должна находить цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'. Используйте конструкцию case.
-- data Maybe a = Nothing | Just a
--findDigit :: [Char] -> Maybe Char

--findDigitOrX :: [Char] -> Char





--Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1. Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и наоборот (вторая функция отбрасывает все элементы списка, кроме первого).








maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

--Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

--    Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
--    Если указаны не все поля, то возвращается IncompleteDataError.
--    Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
--    Если в строке присутствуют лишние поля, то они игнорируются.

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = undefined








main :: IO ()
main = putStrLn "Hello, world!"