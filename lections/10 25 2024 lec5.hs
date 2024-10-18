module Main where
--Генераторы списков
import Prelude hiding (repeat, replicate, cycle, iterate)


--ones :: [Integer]
--ones = 1 : ones

--nats :: Integer -> [Integer]
--nats n = n : nats (n+1)

--f = take 10 $ nats 5
--ff = head $ nats 42

--squares :: [Integer]
--squares = map (^2) $ nats 1

--take 10 $ squares

--Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
--GHCi> take 10 $ fibStream
--[0,1,1,2,3,5,8,13,21,34]

fibStream :: [Integer]
fibStream = zipWith (+) (0:1:fibStream) (0:fibStream)

repeat:: a -> [a]
repeat x = xs where xs = x:xs

replicate :: Int -> a -> [a]
replicate n x = take n $ repeat x

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys where ys = xs ++ ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

--Предположим, что функция repeat, была бы определена следующим образом:
--repeat = iterate repeatHelper

--определите, как должна выглядеть функция repeatHelper.
repeat' :: a -> [a]
repeat' = iterate repeatHelper where
  repeatHelper = id

{--
[1..10]
enumFromTo :: a -> a -> [a]
['a'..'z']
[1,3..10]
enumFromThenTo :: a -> a -> a -> [a]
[1..]
enumFrom :: a -> [a]
[7,14,..]
enumFromThen :: a -> a -> [a]
--}

--let xs = [1..20]
-- [ x^2 | x <- xs]
-- [ x^2 | x <- xs, x > 10]
--[(x,y) | x <- [1..3], y <- [4..6]]

-- Найти все пифагоровы тройки 
--[(x,y,z) | x<-xs, y<-xs, z<-xs, x^2 + y^2 == z^2, x<=y]


--Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию. Воспользовавшись механизмом генераторов списков, напишите функцию change, которая разбивает переданную ей положительную сумму денег на монеты достоинств из списка coins всеми возможными способами. Например, если coins = [2, 3, 7]:
--GHCi> change 7
--[[2,2,3],[2,3,2],[3,2,2],[7]]
--Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы [2,2,3] и [2,3,2] — различаются.

change :: (Ord a, Num a) => a -> [[a]]
change = undefined


{--
data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd n) = Odd (n + 2) 
  pred (Odd n) = Odd (n - 2)
  toEnum n | n `mod` 2 == 1 = Odd (toInteger n)
           | otherwise = error "toEnum: not odd"
  -- | Convert to an 'Int'.
  -- It is implementation-dependent what 'fromEnum' returns when
  -- applied to a value that is too large to fit in an 'Int'.
 -- enumFromTo n m
 --   | n <= m = n : enumFromTo (succ n) m
 --   | otherwise = []
  fromEnum (Odd n) = helper n acc where
    helper :: Integer -> Integer -> Integer
    helper 0 acc = acc 
    helper i acc = helper (i - 1) (acc + 2)

  -- | Used in Haskell's translation of @[n..]@ with @[n..] = enumFrom n@,
  --   a possible implementation being @enumFrom n = n : enumFrom (succ n)@.
  --   For example:
  --
  --     * @enumFrom 4 :: [Integer] = [4,5,6,7,...]@
  --     * @enumFrom 6 :: [Int] = [6,7,8,9,...,maxBound :: Int]@
 
  enumFrom n = n : enumFrom (succ n)
  -- | Used in Haskell's translation of @[n,n'..]@
  --   with @[n,n'..] = enumFromThen n n'@, a possible implementation being
  --   @enumFromThen n n' = n : n' : worker (f x) (f x n')@,
  --   @worker s v = v : worker s (s v)@, @x = fromEnum n' - fromEnum n@ and
  --   @f n y
  --     | n > 0 = f (n - 1) (succ y)
  --     | n < 0 = f (n + 1) (pred y)
  --     | otherwise = y@
  --   For example:
  --
  --     * @enumFromThen 4 6 :: [Integer] = [4,6,8,10...]@
  --     * @enumFromThen 6 2 :: [Int] = [6,2,-2,-6,...,minBound :: Int]@
  --enumFromThen        :: a -> a -> [a]
  -- | Used in Haskell's translation of @[n..m]@ with
  --   @[n..m] = enumFromTo n m@, a possible implementation being
  --   @enumFromTo n m
  --      | n <= m = n : enumFromTo (succ n) m
  --      | otherwise = []@.
  --   For example:
  --
  --     * @enumFromTo 6 10 :: [Int] = [6,7,8,9,10]@
  --     * @enumFromTo 42 1 :: [Integer] = []@
          
  -- | Used in Haskell's translation of @[n,n'..m]@ with
  --   @[n,n'..m] = enumFromThenTo n n' m@, a possible implementation
  --   being @enumFromThenTo n n' m = worker (f x) (c x) n m@,
  --   @x = fromEnum n' - fromEnum n@, @c x = bool (>=) (<=) (x > 0)@
  --   @f n y
  --      | n > 0 = f (n - 1) (succ y)
  --      | n < 0 = f (n + 1) (pred y)
  --      | otherwise = y@ and
  --   @worker s c v m
  --      | c v m = v : worker s c (s v) m
  --      | otherwise = []@
  --   For example:
  --
  --     * @enumFromThenTo 4 2 -6 :: [Integer] = [4,2,0,-2,-4,-6]@
  --     * @enumFromThenTo 6 8 2 :: [Int] = []@
 -- enumFromThenTo      :: a -> a -> a -> [a]
 
  
  
  

  
  

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"
--}

foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' _ ini [] = ini
foldl''' f ini (x:xs) = foldl''' f (f ini x) xs
{--
--}
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ ini [] = ini
foldl'' f ini (x:xs) = foldl'' f ini' xs
  where ini' = f ini x

--более эффективная версия
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ ini [] = ini
foldl' f ini (x:xs) = ini' `seq` foldl' f ini' xs
  where ini' = f ini x

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x b -> p x || b) False
{--
foldr (\x s -> s - x) 0 [1,2,3,4]
-10
foldl (\x s -> s - x) 0 [1,2,3,4]
2


--}
--За один проход по списку вычислить сумму и произведение элементов списка

--foldr (\x (s,p) -> (s+x, x*p)) (0,1) [1,2,3,4]

--Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.
--GHCi> meanList [1,2,3,4]
--2.5
--Постобработка считается допустимой, то есть предполагаемая реализация функции meanList имеет вид
--meanList = someFun . foldr someFoldingFun someIni

meanList :: [Double] -> Double
meanList = (\(a,b) -> a/b) . (foldr (\x (s,p) -> (s+x, p+1)) (0,0))

--meanList = uncurry (/) . (foldr (\x (s,p) -> (s+x, p+1)) (0,0))

--Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.
--GHCi> evenOnly [1..10]
--[2,4,6,8,10]
--GHCi> evenOnly ['a'..'z']
--"bdfhjlnprtvxz"
--foldl' :: (a -> b -> b) -> b -> [a] -> b
--evenOnly :: [a] -> [a]
--evenOnly = foldr (\x xs -> if odd x then x:xs else xs) []
--evenOnly = foldr f []  







main :: IO ()
main = putStrLn "Hello, world!"