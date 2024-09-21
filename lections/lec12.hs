-- module Main where

summSquares x y = x ^ 2 + y ^ 2

{--
summSquares :: Int -> Int -> Int
--}
f x = if x > 0 then 1 else (-1)

g x = (if x > 0 then 1 else (-1)) + 6

max5 x = max 5 x

max5' = max 5

discount limit proc summ = if limit <= summ then summ * (100 - proc) / 100 else summ

standartDiscount = discount 1000 5

newYearDiscount = discount 1000 10

--

xx = 6 :: Int

z = 45656464654654654654654654654654654654654654654 :: Integer

ff :: Char -> Double -> Double
ff a b = (if a == '0' then 1.0 else (-1.5)) + b

ff' :: Double -> Double
ff' = ff '0'

q = True

fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n - 1)

fact' :: Integer -> Integer
fact' 0 = 1
fact' n = if n > 0 then n * fact' (n - 1) else error "arg must be positive"

fact'' :: Integer -> Integer
fact'' n
  | n == 0 = 1
  | n > 0 = n * fact'' (n - 1)
  | otherwise = error "arg must be positive"

-- 8!! = 8*6*4*2
-- 11!! = 11*9*7*5*3*1

fact4 :: Integer -> Integer
fact4 n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be positive"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)





roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  ( (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a),
    (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )





roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c)
   in ((-b - d) / (2 * a), (-b + d) / (2 * a))






roots'' :: Double -> Double -> Double -> (Double, Double)
roots'' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a)
   in (x1, x2)





roots''' :: Double -> Double -> Double -> (Double, Double)
roots''' a b c =
  let x1 = (-b - d) / aTwice
      x2 = (-b + d) / aTwice
      d = sqrt (b ^ 2 - 4 * a * c)
      aTwice = 2 * a
   in (x1, x2)






rootsDiff a b c =
  let (x1, x2) = roots a b c
   in x2 - x1

roots4 a b c = (x1, x2)
  where
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d = sqrt (b ^ 2 - 4 * a * c)
    aTwice = 2 * a

fact5 :: Integer -> Integer
fact5 n
  | n >= 0 =
      let helper acc 0 = acc
          helper acc n = helper (acc * n) (n - 1)
       in helper 1 n
  | otherwise = error "arg must be positive"
  
fact6 :: Integer -> Integer
fact6 n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be positive"
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

x = mod 3 2

y = 3 `mod` 2

zz = 3 + 7

u = (+) 3 7

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x > 0 = helper (0, 0) x
  | x == 0 = (0, 1)
  | otherwise = helper (0, 0) (-x)
  where
    helper (c, s) 0 = (s, c)
    helper (c, s) x = helper ((c + 1), (s + mod x 10)) (div x 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = res
  where
    h = (b - a) / 10000
    res = helper 0 (a + h) 9999
    helper res1 _ 0 = (2 * res1 + f a + f b) * (h / 2)
    helper res1 x n = helper (res1 + (f x)) (x + h) (n - 1)