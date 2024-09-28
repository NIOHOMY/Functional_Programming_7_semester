-- 17) 4, 9, 42, 44, 61, 2.13
{--
4. Дан список из n элементов. Реализовать функцию,
удаляющую из списка k-ый элемент и сохранить его: removeAt
2 "abcd" -> ('b',"acd")
--}
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Index out of bounds"
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) = let (removed, rest) = removeAt (k - 1) xs
                    in (removed, x : rest)

{--
9. Даны координаты трех точек на плоскости. Выяснить,
образуют ли они треугольник. И если да, то проверить,
является ли он прямоугольным. Реализовать функцию,
которая будет возвращать кортеж из пары булевских
значений.
--}
type Point = (Double, Double)

distanceSquared :: Point -> Point -> Double
distanceSquared (x1, y1) (x2, y2) = sqrt ( (x2 - x1) ^ 2 + (y2 - y1) ^ 2 )

triangleProperties :: Point -> Point -> Point -> (Bool, Bool)
triangleProperties p1 p2 p3 = (isTriangle, isRightTriangle)
  where
    d1 = distanceSquared p1 p2
    d2 = distanceSquared p2 p3
    d3 = distanceSquared p3 p1
    
    -- Сумма длин любых двух сторон должна быть больше третьей
    isTriangle = (d1 + d2 > d3) && (d1 + d3 > d2) && (d2 + d3 > d1)
    
    -- Прямоугольный треугольник
    isRightTriangle = isTriangle && 
                      (d1 ^ 2 + d2 ^ 2 == d3 ^ 2 || d1 ^ 2 + d3 ^ 2 == d2 ^ 2 || d2 ^ 2 + d3 ^ 2 == d1 ^ 2)

{--
let p1 = (0, 0)
let p2 = (3, 4)
let p3 = (3, 0)
let (isTriangle, isRightTriangle) = triangleProperties p1 p2 p3
isTriangle
isRightTriangle
--}

{--
42. Дан список из n элементов. Определить, есть ли в списке
хотя бы один элемент, который равен следующему за ним по
кругу элементу. Вернуть True или False.
--}
hasEqualNext :: Eq a => [a] -> Bool
hasEqualNext xs = any equalPair pairs
  where
    pairs = zip xs (tail (cycle xs))
    
    equalPair (a, b) = a == b
{--
44. Дан список целых чисел из n элементов и целое число k.
Посчитать сколько из чисел являются степенями числа k.
--}
isPowerOfK :: Int -> Int -> Bool
isPowerOfK x k
    | k <= 1 = x == 1
    | otherwise = checkPower x
  where
    checkPower n
      | n < 1 = False
      | n == 1 = True
      | otherwise = checkPower (n `div` k) && n `mod` k == 0

countPowersOfK :: [Int] -> Int -> Int
countPowersOfK xs k = length $ filter (`isPowerOfK` k) xs
{--
61. Даны натуральные числа a и b. Определите все
возможные последовательности из a нулей и b единиц, в
которых никакие два нуля не стоят рядом. Вывести все
последовательности в виде списка списков.
--}
generateSequences :: Int -> Int -> [[Int]]
generateSequences a b = backtrack a b []
  where
    backtrack 0 bLeft current = [current ++ replicate bLeft 1]
    backtrack aLeft 0 current
      | aLeft == 1  = [current ++ [0]]
      | otherwise = []  -- Нарушено условине
    backtrack aLeft bLeft current = 
      let withOne = backtrack aLeft (bLeft - 1) (current ++ [1])
          withZero = if null current || last current == 1
                      then backtrack (aLeft - 1) bLeft (current ++ [0])
                      else []  -- Нарушено условине
      in withOne ++ withZero
{--
2.13.  x-(1/4)*x^2 + (1/4)*(3/6)*(x^3) - (1/4)*(3/6)*(5/8)*(x^4) + .. +- 
...*((2*i-3)/(2i))*(x^i) = 2*sqrt(1 + x) - 2
--}
{--
"Неявная" рекурсия
--}
-- Коэфф
coefficients :: [Double]
coefficients = [((-1) ^ i) * (product [2 * (fromIntegral k) - 3 | k <- [2 .. i+1]]) / 
    (product [2 * (fromIntegral k) | k <- [2 .. i+1]]) | i <- [1..]]

-- Отдельные
terms :: Double -> [Double]
terms x = zipWith (*) coefficients (map (x^) [2..])

-- Сумма с E
seriesSum :: Double -> Double -> Double
seriesSum epsilon x = x + (sum $ takeWhile ((>= epsilon) . abs) (terms x))


{--
Явная рекурсия
--}
-- n ый
term :: Int -> Double -> Double
term n x = ((-1)^(n-1)) * (product [2* (fromIntegral k)-3 | k <- [2..n]]) / (product [2* (fromIntegral k) | k <- [2..n]]) * (x^n)

-- Сумма с E
recursiveSeriesSum :: Double -> Double -> Double -> Int -> Double
recursiveSeriesSum epsilon x currentSum n
    | abs nextTerm < epsilon = currentSum
    | otherwise = recursiveSeriesSum epsilon x (currentSum + nextTerm) (n + 1)
  where
    nextTerm = term n x

seriesSumRecursive :: Double -> Double -> Double
seriesSumRecursive epsilon x = x + recursiveSeriesSum epsilon x 0 2
