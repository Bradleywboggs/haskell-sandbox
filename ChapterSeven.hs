--ChapterSeven.hs
module ChapterSeven where
--- 7.1. Make it func-y
-- Overview
-- - Haskell functions are first-class entities that
--   - can be values in expressions, lists, or tuples
--   - can be passed as arguments to a function
--   - can be returned from a function as a result
--   - make use of syntactic patterns

--- 7.2 Arguments and parameters
--  - simple values have no parameters
--  - functions necessarily have parameters that can be applied 
--    to arguments

-- 7.3  Anonymous Functions
--  Named function:
triple :: Integer -> Integer
triple x = x * 3

-- Lambda(Anonymous) Function
-- (\x -> x * 3) :: Integer -> Integer
-- You can also name an anonymous function
trip :: Integer -> Integer
trip = \x -> x * 3

-- To apply anonymous functions, you need to wrap in parens
-- (\x -> x + 5) 6 -> 11

----  Exercises: Grab Bag ----
-- 1. Which are equivalent?
mThA :: Num a => a -> a -> a -> a
mThA x y z = x * y * z

mThB :: Num a => a -> a -> a -> a
mThB x y = \z -> x * y * z

mThC :: Num a => a -> a -> a -> a
mThC x = \y -> \z -> x * y * z

mThD :: Num a => a -> a -> a -> a
mThD = \x -> \y -> \z -> x * y * z
-- All are equivalent
-- mThD could be applied as:
-- (\x -> (\y -> ( \z -> x * y * z) 5) 4) 2 -> 40

-- 2. The type of mTh is Num a => a -> a -> a -> a
--   what is the type of mTh3? : 
--   (d) Num a => a -> a -> a 

-- 3. Rewriting functions
--  a. Rewrite the f function in the where clause
addOneIfOdd :: Integral a => a -> a
addOneIfOdd o = case odd o of
    True -> f o
    False -> o
    where f = \t -> t + 1
-- b. Rewrite as lambda
addFive :: Integral a => a -> a -> a
addFive = \x -> (\y -> (+) (if x > y then y else x) (5))
-- This doesn't work as expected. The conditional statement always returns x even if x > y
--c . Rewrite without lambdas
mflip :: (a -> b -> c ) -> b -> a -> c
mflip f = \x -> \y -> f y x

myFlippy :: (a -> b -> c ) -> b -> a -> c
myFlippy f x y = f y x

--- 7.4 Pattern Matching
--  Pattern matching is a way of matching values against patterns and,
--  where appropriate, binding variables to successful matches.
--  Patterns are matched against VALUES, that is, DATA CONSTRUCTORS, not TYPES

--  Matching a pattern may fail, which results in trying to match on the next potential
--  match. WHen a match succeeds, teh variables exposed in teh pattern are bound.
--  Pattern Matching proceeds from LEFT to Right and outside to inside.

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- You can Pattern match in Prelude using the :{ }: syntax.
--      :{
--          ...function type sig, pattern matches...
--      }:

newtype Username = 
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User = 
    UnregisteredUser 
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Sign up!"
printUser (RegisteredUser 
            (Username name)
            (AccountNumber acctNum)) =
        putStrLn $ name ++ " " ++ show acctNum
    
data WherePenguinsLive = 
    Galapapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = 
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

macaroni :: Penguin
macaroni = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p)
    || (antarcticPenguin p)

-- Note: || in a function def is a logical 'or' like other langs
--       | is used in data declarations to separate Sum Types

-- Pattern Matching Tuples
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEm2Alt :: Num a => (a, a) -> a
addEm2Alt tuple = (fst tuple) + (snd tuple)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, z) = z

---- Exercises: Variety Pack ----
--1.
k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4-1), 10)

k2 :: [Char]
k2 = k ("three", (1+2))

k3 :: Num a => a
k3 = k (3, True)

--2.
threeTupsToTwoTups :: (a, b, c)
                   -> (d, e, f)
                   -> ((a,d), (c, f))
threeTupsToTwoTups (q,r,s) (t,u,v) = ((q,t), (s, v))

-- 7.5 Case Expressions
-- another way of expression conditionals when you have exposed Data constructors
-- basically prettier, more robust version of switch

funcy :: (Eq a, Num a) => a -> [Char]
funcy x = if x + 1 == 1 then "Awesome" else "wut"

funcz :: (Eq a, Num a) => a -> [Char]
funcz x = 
    case x + 1 == 1 of
        True -> "AWESOME"
        False -> "wut"

pal :: Eq a => [a] -> [Char]
pal xs =
    case (xs == reverse xs) of
        True -> "palindromic"
        False -> "not so much"

palindromitus :: Eq a => [a] -> [Char]
palindromitus xs =
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness = 
    case cool of 
        True -> 
            putStrLn "Hi Cool person"
        False -> 
            putStrLn "..."
    where cool =
            coolness == "very cool"
---- Exercises: Case Practice ----
-- 1.
greaterThan :: (Ord a) => a -> a -> a
greaterThan x y = 
    case x > y of
        True -> x
        False -> y
-- 2. 
add2IfEven :: Integral a => a -> a
add2IfEven num =
    case even num of
        True -> (+) num 2
        False -> num
-- 3.
compNum :: Int -> Int
compNum x = 
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

-- 7.6 Higher-order functions
-- Functions that accept other functions as arguments
-- to specify that a 'group' of arguments is actually a 
-- single function argument, wrap in parens

flop :: (a -> b -> c) -> b -> a -> c
flop func x y = func y x

-- heed :: [a] -> Maybe a

data Employee = Coder
               | Manager
               | Veep
               | CEO
               deriving (Eq, Ord, Show)
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = 
    putStrLn $ show e ++
               " is the boss of " ++
               show e'
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = 
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee\
                        \ is the boss"
        LT -> (flip reportBoss) e e'

-- We can paramaterize the comparing function (a -> a -> Ordering)

employeeRanking :: ( Employee 
                  -> Employee 
                  -> Ordering )
                  -> Employee
                  -> Employee
                  -> IO ()
employeeRanking f e e' = 
    case f e e' of
     GT -> reportBoss e e'
     EQ -> putStrLn "Neither employee\
                     \ is the boss"
     LT -> (flip reportBoss) e e'

codersFTW :: Employee
          -> Employee
          -> Ordering
codersFTW Coder Coder = EQ
codersFTW _ Coder = LT
codersFTW Coder _ = GT
codersFTW e e' = compare e e'
---- Exercises: Artful Dodgy ---- 
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

--1. 
--  dodgy 1 0 evals to 1 + 0 * 10. // 1


--------------------- 7.11 Chapter Exercises ---------------------
---- Multiple Choice
-- 1. A Polymorphic function : (d) may resolve to values of different types
--    depending on the inputs
-- 2. Two fns:
--      f :: Char -> String
--      g :: String -> [String]
--    The composed function g . f has the type: (b) Char -> [String]
-- 3. A fn:
fa :: Ord a => a -> a -> Bool
fa x y = x > y
--    One numeric value has been applied to it. what is the type now?
--    () fNum :: (Ord a, Num a) => a -> Bool

-- 4. A fn:
--    yolo :: (a -> b) -> c
--   (b) is a higher order function

-- 5. Given the following def of za: what is the type of za True
za :: a -> a
za x = x
-- (a) za True :: Bool

---- Let's Write Code
-- 1. The following function retunrs the tens digit of an integral argument
tensDigit :: Integral a => a -> a
-- tensDigit x = d
--    where xLast = x `div` 10
--          d     = xLast `mod` 10
-- initial rewrite:
tensDigit x = mod (div x 10) 10
-- a) rewrite it using divMod
-- b) does the divMod version have same type as the original?
-- c) Change it so that we're getting the hundreds digit instead