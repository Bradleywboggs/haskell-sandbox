--ChapterFour.hs

module ChapterFour where

--pg. 89-90 Exercise "Mood Swing"

-- data signifies a data declaration
-- Mood is the Type name or Type Constructor
-- Woot, Blah are the data constructors and represent the actual values possible for the Mood type
data Mood = Woot | Blah deriving Show
-- the changeMood function is being declared to take Mood value and return Mood a value
-- these must be chosen from the data constructors
changeMood :: Mood -> Mood

-- here we see that changeMood is taking a Mood type value, Blah and returning the other Mood type value Woot
changeMood Blah = Woot

--Here is essentially the opposite operation
changeMood _    = Blah

--Tuples - the function type signature is nearly identical to the function definition.
-- You can define the types very generally as any type, i.e. 'a,b', providing lots of 
-- flexibility. 
fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a,b) -> b
snd' (a, b) = b 
-- You can also specify that it will only be of a specified type as below,
-- this provides a greater reasonability to program as the compiler will enforce 
-- only the provided data types.
topFunc :: (Int, [a]) 
        -> (Int, [a]) 
        -> (Int, [a])
topFunc (a,b) (c,d) = ((a+c), (b++d))

-- Exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
--1. Given the definition of length 'a function that takes a list and returns 
--   result that tells how many items are in the list' It's type signature would be
--   length :: [a] -> Num -- GHCI answer --> length :: Foldable t=> t a -> Int.
--   It takes 1 argument of List type and returns an Int.

--2. What are the results of the following expressions?
twoA = length [1,2,3,4,5] --5
twoB = length [(1, 2), (2, 3), (3, 4)] -- 3
twoC = length allAwesome --2
twoD = length $ concat allAwesome --5

--3. Which compiles?
three = 6 / 3 --compiles

--   6 / length [1,2,3] does not compile because (/) requires values which have 
--   an instance of the Fractional typeclass, the length function returns Int
--4 Fix the non-compiling operation above:
four = div 6 $ length [1, 2, 3]

--5. The expression:
five = 2 + 3 == 5 --returns a Bool type, in this case the data constructor 'True'

--6. What is the (a) type and (b) expected result of the following?
sixA :: [Char]
sixB :: ([Char], Bool)
sixVar = 5
sixA = if (sixVar + 3 == 5) == True || (sixVar + 3 == 5) == False then "Boolean!" else "Nopers"
sixB= (,) sixA  (sixVar + 3 == 5) --  Bool, False

--7. Which will compile?
sevenA = length allAwesome == 2 -- True
-- length [1, 'a', 3, 'b'] -- won't compile multiple datatypes in one list
sevenB = length allAwesome + length awesome --5
sevenC = (8 == 8) && ('b' < 'a') -- False
-- (8 == 8) && 9 -- won't compile ("No instance for (Num Bool)...)

--8. Palindrome checsk function
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if reverse x == x then True else False

--9. Absolute value function
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x 

--10. Write the function
f :: (a,b) -> (c,d) ->((b,d),(a,c))
f (a,b) (c,d) = ((b,d), (a,c))

--CORRECTING SYNTAX
--1. Add one to the length of a String arg and return the result
x = (+)
addOne s = x w 1
     where w = length s

--2. Identity function
identity :: a -> a
identity x = x 

--3. Fix the "first" function
first :: (a,b) -> a 
first (a,b) = a 

--MATCH FUNCTION NAMES TO TYPES
-- show :: Show a => a -> String

-- (==) :: Eq a => a -> a -> Bool

-- fst :: (a,b) -> a 

-- (+) :: Num a => a -> a -> a 
