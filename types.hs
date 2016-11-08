import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _  r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = ( abs $ x2 - x1) * ( abs $ y2 -y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


-- record syntax for better declaration and accessors
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)
-- can use year car-inst to get at a car instance's year value


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken 
                            then Right code 
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
            [(100,(Taken, "1234"))
            ,(101,(Free, "asdf"))
            ]

-- lets implement our own list
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) ::  List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- our own tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

data TrafficLight = Red | Yellow | Green

-- This is how we implement our own interpretations of different type class functions
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "STAHP"
    show Yellow = "GO FASTR"
    show Green = "Green light"

-- This pretty much works, doesnt handle Red < Red or Green < Green. Better to just use "deriving (Ord)"
instance Ord TrafficLight where
    Red < _   = True
    Yellow < Green = True
    Green < _ = False
    _ < _ = False
    a <= b = a < b || a == b
    a >= b = b <= a
    a > b = b < a

-- Type classes are like templates. The type class elevates the type. It itself is not a type.
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

-- this is how you create an instance with a Maybe. Maybe isnt a type either, but a (Mabye a) is
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- something slightly more complicated
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

-- functor is defined as
-- class Functor f where
--      fmap :: (a -> b) -> f a -> f b
--
-- an interesting example is the Maybe instance:
-- instance Functor Maybe where
--  fmap f (Just a) = Just (f a) -- given a function that doesnt speak Maybe and a Maybe, provide the Maybe val to func
--                               -- and return the function applied to the val wrapped in the Maybe
--  fmap f Nothing = Nothing     -- Do nothing real in this case
-- This is cool: We can make the tree a functor, so that we can apply a function over the entire thing:
-- Notice we dont have an instance statement with a type, but a type constructor
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
