-- i: Semigroup

data Point = Point Double Double 
    deriving (Show, Eq, Ord)

instance Semigroup Point where
    (<>) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

{--
let point1 = Point 4 5
let point2 = Point 15 9
point1 <> point2

Point 19.0 14.0
--}

instance Monoid Point where
    mempty = Point 0 0
    mappend = (<>)
    mconcat = foldl mappend mempty
    
    
type Name = String

type IntList' = [Integer]
newtype IntList = IntList [Integer]

data Point' = Point' !Double !Double -- не ленивые параметры
    deriving (Show, Eq, Ord)

getPointX (Point x _) = x
getPointX' (Point' x _) = x

getPointX'' ~(Point x _) = 0
getPointX''' _ = 0 -- если нет сопоставления, сработает для undefined

data MyList a = ListEmpty | a :% (MyList a)
    deriving Show

{--
5 :% (3 :% ListEmpty)
--}

myHead (x :% _) = x

{--
ghci> myHead (5 :% (3 :% ListEmpty))
5
ghci> myHead $ 5 :% (3 :% ListEmpty)
5
--}

class Area g where
    area :: g -> Double
    semiArea :: g -> Double
    semiArea g = area g / 2

instance Area Point where
    area _ = 0

{--
ghci> let point2 = Point 15 9
ghci> semiArea point2
0.0
--}

-------------------------------------------------------
-- :i Functor
-- Maybe/List with context

dist (Point x y) = sqrt $  x^2 + y^2 

{--
ghci> let point2 = Point 15 9
ghci> dist point2
17.4928556845359
--}

maybePoint1 = Just (Point 3 4)
maybePoint2 = Nothing


maybeDist (Just point) = Just $ dist point 
maybeDist Nothing = Nothing

-- дз список целых чисел, реализовать класс типов маноид, где будет 2 реализации: 1 - складывать поэлементно 2 - перемножать поэлементно
-- написать (реализацию классов типов) класс типа Functor для классов типов Maybe
-- аналогично для Either (Left|Right фактическое значение) // почему в такой последовательности

{--
fmap dist maybePoint2
--}

newtype AdditiveIntList = AdditiveIntList [Integer]
    deriving Show

instance Semigroup AdditiveIntList where
    (AdditiveIntList xs) <> (AdditiveIntList ys) = AdditiveIntList (addLists xs ys)
      where
        addLists [] [] = []
        addLists (x:xs) [] = x : xs
        addLists [] (y:ys) = y : ys
        addLists (x:xs) (y:ys) = (x + y) : addLists xs ys


instance Monoid AdditiveIntList where
    mempty = AdditiveIntList []


newtype MultiplicativeIntList = MultiplicativeIntList [Integer]
    deriving Show

instance Semigroup MultiplicativeIntList where
    (MultiplicativeIntList xs) <> (MultiplicativeIntList ys) = MultiplicativeIntList (zipWith (*) xs ys)


instance Monoid MultiplicativeIntList where
    mempty = MultiplicativeIntList []


{--
let list1 = AdditiveIntList [1, 2, 3]
let list2 = AdditiveIntList [4, 5, 6, 7, 8]

mappend list1 list2
mappend list1 mempty


let list3 = MultiplicativeIntList [1, 2, 3]
let list4 = MultiplicativeIntList [4, 5, 6, 7, 8]

mappend list3 list4
mappend list3 mempty

--}


newtype MyMaybe a = MyMaybe (Maybe a)
    deriving (Show, Eq)

instance Functor MyMaybe where
    fmap _ (MyMaybe Nothing)  = MyMaybe Nothing
    fmap f (MyMaybe (Just x)) = MyMaybe (Just (f x))

newtype MyEither a b = MyEither (Either a b)
    deriving (Show, Eq)

instance Functor (MyEither a) where
    fmap _ (MyEither (Left x))  = MyEither (Left x)
    fmap f (MyEither (Right x)) = MyEither (Right (f x))

{--
fmap (*2) (MyMaybe (Just 2))
fmap (*2) (MyMaybe Nothing)
   
fmap (*2) (MyEither (Right 3))
fmap (*2) (MyEither (Left "Error"))

--}

