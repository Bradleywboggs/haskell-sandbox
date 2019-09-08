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
-- (\x -> x + 5) 6  -- 11

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
addOneIfOdd o = 
    case odd o of
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

-- Pattern mathing is a syntactc way of deconstructing products and sum
-- types to get their inhabitants.

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

data Blah = Blah deriving Show
-- pattern matching on Blah does 1 thing

blahFunc :: Blah -> Bool
blahFunc Blah = True

data Identity a = 
    Identity a 
    deriving (Eq, Show)
-- Unary data constructor( one Data constructor)
-- Product Type
unpackId :: Identity a -> a
unpackId (Identity x) = x

data FirstName = FirstName String deriving Show
data LastName =  LastName String deriving Show
data Age = Age Int deriving Show
data Person = Person FirstName LastName Age deriving Show

getFName :: Person -> FirstName -> FirstName
getFName (Person 
           (FirstName name)
           (LastName lastName)
           (Age num))
           (FirstName _) = (FirstName name)
           
getLName (Person 
           (FirstName name)
           (LastName lastName)
           (Age num))
           (LastName _) = (LastName lastName)

pluckFName :: FirstName -> String
pluckFName (FirstName name) = name

pluckLName :: LastName -> String
pluckLName (LastName name) = name

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

--1. dodgy 1 0 evals to 1 + 0 * 10. // 1
--2. dodgy 1 1 // 11
--3. dodgy 2 2 // 22
--4. dodgy 1 2 // 21
--5. dodgy 2 1 // 12
--6. oneIsOne 1 -> 1 + (1) * 10 // 11 
--7. oneIsOne 2 // 21
--8. oneIsTwo 1 // (1) + 2 * 10 // 21
--9. oneIsTwo 2 // 22
--10. oneIsOne 3 // 31
--11. oneIsTwo 3 // 23
------------------------------------
---- 7.6b Writing Guard Blocks ----
-- alternative to if-then-else or ternary
-- each guard has its own equal sign and there isn't one
-- in the first line of the function definition becasuse each case needs its own
-- expression
-- is this simply an alternative to case ? Or there a limit to one not present in 
-- the other?
myAbs :: Integer -> Integer
myAbs x
    | x < 0     = (-x)
    | otherwise = x
-- | begins each guard case
-- otherwise is alias for True
-- Each expression within the guard block must be a predicate

bloodNa :: Integer -> String
bloodNa x 
    | x < 135   = "too low"
    | x > 145   = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) 
        => a -> a -> a -> String
isRight a b c 
    | a^2 + b^2 == c^2 = "Right on"
    | otherwise        = "not right"

dogYrs :: Integer -> Integer
dogYrs x
    | x <= 0    = 0
    | x <= 1    = x * 15
    | x <= 2    = x * 12
    | x <= 4    = x * 8
    | otherwise = x * 6

avgGrade :: (Fractional a, Ord a)
        => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x /100
----- Exercises: Guard Duty -----
--1. 
--2. 
-- Bottom line for 1 and 2: you must be very specific in 
-- How you order your guards.
--3. (C) When xs is a palindrome
pally :: Eq a => [a] -> Bool
pally xs
    | xs == reverse xs = True
    | otherwise        = False
--4. What tyoes of args can pal take? Eq a.
--5. What is the type of function pally?
--  pally :: Eq a => [a] -> Bool
--6. 
numbbies :: (Ord a, Num a) => a -> Int
numbbies x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
-- (c) returns an indication of positive or negative
-- why type of args can it take? What is the function type
-- Function composition
add :: Int -> Int -> Int
add x y = x + y

---- 7.8 Function composition ----
-- infix operator (.)
-- has lower precedence than whitespace
-- must be applied to a function before the function is applied to an argument
-- type sig:
--  (.) :: (b -> c) -> (a -> b) -> a -> c
--    - given a function from b -> c
--    - given a function from a -> b
--    - return a function from a -> c

negativeSum :: Num a => [a] -> a
negativeSum list = negate . sum $ list

takeFromReversed :: [a] -> [a]
takeFromReversed list = take 5 . reverse $ list

fiveOddFromNum :: Integral a => a -> [a]
fiveOddFromNum x = take 5 . filter odd . enumFrom $ x
-- RETURN VAL Integral   a => [a]  
--                 ([a] -> [a]) . 
--                 (Integral a => [a] -> [a]) .
--                 (Enum a => a => [a])
-- INPUT VAL Integral a => a

---- 7.9 Pointfree style ----
-- a 'point' in this context refers to a function argument
-- So 'pointfree' style means defining functions, 
--  omitting the arguments, allowing them to be inferred
--   making a more concise (terse?) syntax
--  It helps the reader focus on the functions rather than the data
sumTheList :: Int -> [Int] -> Int
sumTheList x xs = foldr (+) x xs 

sumTheListPF :: Int -> [Int] -> Int
sumTheListPF = foldr (+)

aCounter :: [Char] -> Int
aCounter = length . filter (== 'a') 


data InvStatus = Unprocessed | Processed deriving (Eq, Show)
data Invoice  = Invoice InvStatus deriving (Eq, Show)

-- getStatus :: Invoice -> InvStatus
-- getStatus invoice = 
--     case invoice == Invoice Processed of
--         True -> Processed
--         False -> Unprocessed
-- process :: Invoice -> Invoice
-- process invoice =
--     case (getStatus invoice) == Unprocessed of
--         _ -> Invoice Processed

adder :: Int -> Int -> Int
adder x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

compo :: IO ()
compo = do
    print (0 :: Int)
    print (add 1 0)
    print (addOne 0)
    print (addOnePF 0)
    print ((addOne . addOne) 0)
    print ((addOnePF . addOnePF) 0)
    print (negate (addOne 0))
    print ((negate . addOne) 0)
    print ((addOne . addOne . addOne . negate . addOne) 0)
    -- 0
    -- 1
    -- 1
    -- 1
    -- 2
    -- 2
    -- -1
    -- -1
    -- 2

    ---- 7.10 Demonstrating Composition ----
    -- putStrLn :: String -> IO ()
    -- show :: Show a => a -> String
    -- print :: Show a => a -> IO ()
    -- print demonstrates a composition of putStrLn and show
    
    -- print x = putStrLn . show $ x
    -- print = putStrLn . show

  
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
tensDigitFancy x = flip mod 10 $ div x 10
-- a) rewrite it using divMod
tensDigitDivMod0 :: Integral a => a -> a
tensDigitDivMod0 x = snd $ divMod x 10
-- returns the Ones digit?
tensDigitDivMod1 :: Integral a => a -> a
tensDigitDivMod1 x = snd . flip divMod 10 $ fst $ divMod x 10
-- This is the most bloated and unnecessary refactor of this function
-- that I could imagine. 
-- perhaps just this?...
tensDigitDivMod2 :: Integral a => a -> (a, a)
tensDigitDivMod2 x = flip divMod 10 $ div x 10
-- b) does the divMod version have same type as the original?
--    it can. But out of the box, no
-- c) Change it so that we're getting the hundreds digit instead
hundsDigit :: Integral a => a -> a
hundsDigit x = flip mod 10 $ div x 100

-- 2.
-- a. implement foldBool with case statement
foldBoolA :: a -> a -> Bool -> a
foldBoolA x y bool =
    case bool of
        True -> y
        False -> x
-- b. implement foldBool with guards
foldBoolB :: a -> a -> Bool -> a
foldBoolB x y bool
    | bool == False = x
    | bool == True = y

--3. fill in the def
blahhh :: (a -> b) -> (a, c) -> (b, c)
blahhh func (val1, val2) = (,) (func val1) val2

--4. write point free versions

roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read $ show a
roundTrip = read . show

mainRT = do
    print (roundTrip 4)
    print (id 4)

--5. another roundTrip
roundTrip0 :: (Show a, Read b) => a -> b

roundTrip0 a = read $ show a
mainRT0 = do
    print $ (roundTrip0 4 :: Int)



