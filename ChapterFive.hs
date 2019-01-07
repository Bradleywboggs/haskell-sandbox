--ChapterFive.hs

module ChapterFive where
--OBJECTIVES:
    -- Query and read type Sigs
    -- Define Currying and Explain with examples
    -- Describe different kinds of Polymorphism
    -- Define Type Inference
    -- Declare types for new functions


--pg 125-126 Exercises

--Type Matching
--1a,2c
--not :: Bool -> Bool

--1b,2d
--length :: Foldable t => t a -> Int

--1c, 2b
--concat :: Foldable t => t [a] -> [a]

--1d,2a
--head :: [a] -> a

--1e,2e
--(<) :: Ord a => a -> a -> Bool

--5.4 Currying
  -- Currying refers to the nesting of mulitiple functions,
  -- each accepting ONE argument and return ONE result
  -- there can be ONE argument, ONE result per ->
    -- f :: a -> a -> a associates to 
    -- f :: a -> (a -> a)
    -- map :: (a -> b) -> [a] -> [b] associates to
    -- map :: (a -> b) -> ([a] -> [b]
  -- The right associativity (Right to Left)doesn't
  -- affect order of op. It groups params into 
  -- ARG and RESULT.
  -- ?? (+) Num a => a -> (a -> a)
  --   is it like an identity function in the first application?
  --   (+) 3 -> 4 -> 7???  How does the 4 represent the first
  --   application  of the function??? 


  --Partial Aplication -p 161

  addStuff :: Integer -> 
              Integer -> 
              Integer
  addStuff a b = a + b + 5

  addTen :: Integer -> Integer
  addTen = addStuff 5
  addTenExplained b =  5 + b + 5 
  -- addStuff is partially applied, being given only one argument, 'a'. Thus, it still needs the b
  -- by creating a new function from this partial application you get - addTen a = a + (5) + 5
  fifteen :: Integer
  fifteen = addTen 5
  fifteenAlt = addStuff 5 5
  fifteenExplained = 5 + 5 + 5
  
  subtractStuff :: Integer ->
                   Integer ->
                   Integer
  subtractStuff x y = x - y - 10

  subtractOne :: Integer -> Integer
  subtractOne = subtractStuff 1
  subtractOneExplained y = 1 - y - 10

  result :: Integer
  result = subtractOne 11
  resultExplained = 1 - 11 - 10
  
--Uncurrying 130-133
-- Uncurrying means un-nesting the functions and 
-- replacing the two functions with a tuple of two values.
--   Uncurried Functions: 1 function, many args
--   Curried functions: Many functions, 1 arg/ea
--     (+) :: Num a => a -> a -> a
--     uncurry (+) :: Num a => (a, a) -> a

  nonsense :: Bool -> Integer
  nonsense True = 805
  nonsense False = 31337


  curriedFunction :: Integer
                  -> Bool
                  -> Integer
  curriedFunction i b = i + (nonsense b)


  uncurriedFunction :: (Integer, Bool)
                    -> Integer
  uncurriedFunction (i, b) = i + (nonsense b)


  anonymous :: Integer 
             -> Bool 
             -> Integer
  anonymous = \i b -> i + (nonsense b)


  anonNested :: Integer 
             -> Bool 
             -> Integer

  anonNested = \i -> \b -> i + (nonsense b)

  -- Currying and Uncurrying Existing Functions pg 164
  currie f a b = f (a, b)
  
--pg 134-135 Exercises

--1. 
  -- let f :: a -> a -> a -> a; f = undefined
  -- let x :: Char; x = undefined 
  -- f x :: Char -> Char -> Char -> Char

--2 
  -- let g :: a -> b -> c -> b; g = undefined
  -- g 0 'c' "woot" :: Char

--3
  --let h :: (Num a, Num b) => a -> b -> b; h = undefined
  --h 1.0 2 :: Num b => b

--4
  --h 1 (5.5 :: Double) :: Double

--5
  -- let jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
  -- jackal "keyboard" "has the word jackal in it" :: [Char]

--6
 


