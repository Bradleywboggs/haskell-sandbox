--ChapterSix.hs
module ChapterSix where
import Data.List

-- 6.1 Typeclasses
--  - Examine the typeclasses Eq, Num, Ord, Enum and Show
--  - learn about type-defaulting typeclasses and typeclass inheritance
--  - look at some common but often implicit functions that create side effects


-- 6.2 What are typeclasses
--  -  A type declaration defines how a PARTICULAR type is created
--  -  A typeCLASS declaration defines how a SET of types are CONSUMED or
--  -  used in computations

-- 6.3 Back to Bool
-- -   Typeclass hierarchy:  
--         Eq, Ord, Enum (general to specific)
--         To put something in an enumerated list they must be ordered
--         To order something they be able to be compared for equality

--  6.4 Eq

-- 6.5 Writing Typeclass instances:

data Trivial = Trivial

instance Eq Trivial where
    Trivial == Trivial = True

data DayOfWeek =
    Mon | Tues | Weds | Thu | Fri | Sat | Sun deriving Show
-- day of week and numerical day of month

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tues Tues = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date =
        Date DayOfWeek Int deriving Show

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday'
        && dayOfMonth == dayOfMonth'
data Identity a =
    Identity a deriving Show
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = 
    TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn integer) (TisAn sameInteger) = integer == sameInteger

data TwoIntegers = 
    Two Integer Integer deriving Show
instance Eq TwoIntegers where
    (==) (Two intOne intTwo) (Two otherIntOne otherIntTwo ) = 
        intOne == otherIntOne && intTwo == otherIntTwo

data StringOrInt =
    TisAnInt Int | TisAString String deriving Show
instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt otherInt) = int == otherInt
    (==) (TisAString string) (TisAString otherString) = string == otherString
    (==) (TisAnInt int) _ = False
    (==) (TisAString string) _ = False

data Pair a =
    Pair a a deriving Show
instance Eq a => Eq (Pair a) where
    (==) (Pair val val2) (Pair val3 val4) =
        val == val3 && val2 == val4

data Tuple a b =
    Tuple a b deriving Show
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple val1 val2) (Tuple val3 val4) =
        val1 == val3 && val2 == val4

data Which a =
    ThisOne a
    | ThatOne a deriving Show
instance Eq a => Eq (Which a) where
    (==) (ThisOne val) (ThisOne otherVal) =
        val == otherVal
    (==) (ThisOne val) _ = False
    (==) (ThatOne val) (ThatOne otherVal) =
        val == otherVal
    (==) (ThatOne val) _ = False

data EitherOr a b =
    Hello a
    | Goodbye b deriving Show
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello val) (Hello other) =
        val == other
    (==) (Hello val) _ = False
    (==) (Goodbye val) (Goodbye other) =
        val == other
    (==) (Goodbye val) _ = False
-- Num 6.6 Num
--  Here we see a variety of Numeric types all of which have instances of the Num Typeclass
--  When a type has an instance of Num, it inherits all of Num's function defs. A type MAY NEVER o
--  override Num's defs. 
--Tuple experiment
ones x = snd (divMod x 10)

-- Note on a common GHCI error: 'No Instance for Show..."" 
-- If you try to partially apply a function, without assigning the partially applied function
-- a name(you simply enter max 3), you're invoking the print function -
-- print:: Show a => a -> IO(). functions don't have an instance of Show.

-- Exercises: Will they work?
-- 1. max (length [1,2,3]) (length [1,2,3,4,5])
--    Compiles. Evals to: 5
-- 2. compare (3 * 4) (3 * 5)
--    Compiles. Evals to: LT 
-- 3. compare "Julie" True
--    Doesn't compile. [Char] and Bool not comparable
-- 4. (5 + 3) > (3 + 6)
--    Compiles. Evals to False

-- 6.9 Enum
-- Interesting functions:
-- enumFromTo:: Ord a => a -> a -> [a]
--  eg. enumFromTo 3 8 -> [3,4,5,6,7,8]
-- enumFromThenTo: Ord a => a -> a -> a -> [a]
-- eg enumFromThenTo 1, 5, 20 -> [1,5,9,13,17]
--    The the first and last 'a' represents the total range (exclusive), 
--    the middle 'a' represents the 'step'

-- 6.12 Instances are dispatched by type
--   Typeclasses are defined by the set of operations and values all instances
--   will provide. Typeclass instances are UNIQUE PAIRINGS of the Typeclass and the Type
--   Each concrete Type can implement the Typeclass's operations in its own specific way

--   -- A Typeclass defines a set of functions and/or values
--   -- Types have instances of that typeclass
--   -- The instances specify the ways that types uses the functions
--      of the typeclass

--   Type inference - When you give enough information the compiler
--     is able to figure out your data types   

--   Type assertion - sometimes you need to assert your type directly
--     defaultNumber :: Age -> Age 65
--     defaultNumber :: Year -> Year 1988

-- 6.13 Gimme More operations
--  Fewer constraints (thus less knowledge of concrete types) yields fewer operations
--  More constraints (thus more knowledge of concrete types)
--  yields more possible operations

--   someOperation :: a -> a -> a
--      -- can only return itself
--   numOperation :: Num a => a -> a -> a
--      -- can be any numeric operation with two args
--   ordNumOperation :: (Ord a, Num a) => a -> a -> a
--      -- can be any numeric operation that also has ordering
--   intOperation :: Int -> Int -> Int
--     -- can be any num, integral, ord, enum, bounded operation

-- Concrete types imply all the typeclasses they provide
--  if you define functions using Concrete types, all the typeclass 
--    constraints are there already
--  This can be handy. As a general principle it's better to define
--  functions in the most generic way possible. Then you keep your
--  focus on what the operation DOES to any given data, rather than
--  boxing yourself in to thinking about the type of data you're currently working with
--  additionally, if you define functions with concrete types, you invite 
-- unintended behaviors. Int's range of behaviors/operations is very broad,
-- and it's unlikely that your function's behavior doesn't span all of Int's behavior

--------------------- 6.14 Chapter Exercises ---------------------

----- Multiple Choice
-- 1. The Eq class: (c) makes equality tests possible
-- 2. The typeclass Ord: (a) allows any two values to be compared
-- 3. Suppose the typeclass Ord has an operator (>). What is the type of (>)?
--    (a) (>) :: Ord a => a -> a -> Bool
-- 4. In x = divMod 16 12: (c) the type of x is a tuple
-- 5 The typeclass Integral includes: (a) Int and Integer numbers

----- Does it Typecheck?
-- 1. Initially doesn't compile until Show is implemented
data Person = Person Bool deriving Show 
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- 2.Initially doesn't compile, as it didn't have an instance of Eq
data Mood = Blah
            | Woot deriving Show
instance Eq Mood where
    (==) Blah Blah = True
    (==) Blah _ = False
    (==) Woot Woot = True
    (==) Woot _ = False
-- This can just be done with 'deriving (...,Eq) in the data constructor
-- But I did it the long way for practice.
settleDown x = if x == Woot 
                  then Blah
                  else x
-- 3. 
--  a. What values are acceptable inputs to settleDown x?
--     Woot | Blah
--  b. If you try to run settleDown 9, you'll get a compiler error
--     because the Mood data type is inferred in the function definition
--  c. If you try to run Blah > Woot, you'll get a compiler error
--     because Mood doesn't have an instance of Ord

-- 4. Initially doesn't compile because s1 is missing "Object" argument
--    which is required by the type constructor. 
--      How could I implement use of Maybe here?

type Subject = String
type Verb = String
type Object = String

data Sentence = 
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "eat" "cats"
s2 = Sentence "Julie" "loves" "dogs"

---- Given a datatype declaration what can we do?
data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu = 
    Papu Rocks Yeah
    deriving (Eq, Show)

----  Does it Typecheck 2
-- 1. Doesn't compile. Type inferred: Papu. Malformed Data types. Should be
--    Papu (Rocks "chases") (Yeah True)
-- phew = Papu "chases" True

-- 2. Compiles
truth = Papu (Rocks "chomskydoz")
             (Yeah True)
-- 3. Compiles.
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4. Doesn't compile. No instance of (Ord Papu)
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

---- Match the Types
-- 1. Cannon subsitute the second type sig for the first. 
i :: Num a => a
-- -- or i :: a
i = 1

-- 2. Cannot substitute. Float has Num. But Num does not have Float
f :: Float
-- f:: Num a => a
f = 1.0

-- 3. Can substitute. Float has instance of Fractional, which is sufficient for 1.0
-- g :: Float
g :: Fractional a => a
g = 1.0

-- 4. Can substitute. Float has instance of RealFrac, which is sufficient for 1.0
-- h :: Float
h :: RealFrac a => a
h = 1.0

-- 5. Can substitute. First definition is as generic as possible - simply a constant.
--     A more specific form is fine (albeit unnecessary)
freud :: a -> a
-- freud :: Ord a => a -> a
freud x = x

-- 6.  Can substitute. But why? Int is an unnecessary constraint 
--     on simply returning a const.
freud' :: a -> a
-- freud' :: Int -> Int
freud' x = x

-- 7. Cannot substiute. myX  is concretely defined as Int, 
--    and cannot be used in a generic a -> a function.
myX = 1 :: Int
-- sigmund :: Int -> Int
-- sigmund :: a -> a
-- A different implementation that works. It simply returns 1
--   given any input
sigmund :: a -> Int
sigmund x = myX

-- 8. Cannot substitute. myY is concretely defined as Int,
--    and cannot be used as a generic Num a => a
myY = 10 :: Int
-- sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
-- more generic implementation that compiles
sigmund' :: Num a => a -> Int
sigmund' y = myY

-- 9. Can substitute. Int has Ord.
jung :: Ord a => [a] -> a
-- jung :: [Int] -> Int
jung xs = head (sort xs)

--10. Can substitute. Ord is only thing needed for the operation
--     Char has ord
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

--11. Cannot substitue. mySort has concrete types [Char] and [Char]
--    mySort is a dependency of signifier, so signifier must have the same
--    concrete types
mySort :: [Char] -> [Char]
-- This is interesting. Specifying the concrete types
-- then assigning mySort to sort, means that mySort is 
--  a concrete implementation of sort
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

---- Type-Kwon-Do Two: Electric Typealoo
-- Derive the implementation from the type signature
-- 1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn alpha beta = fn alpha == beta

-- 2.
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith operation intOperand anyOperand = (operation anyOperand) ^ intOperand







