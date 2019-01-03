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
 


