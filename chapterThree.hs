--chapterThree.hs

module ChapterThree 
    ( rvrse, afterTheNine )
    where
    
    -- Chapter examples
    
    main :: IO ()
    
    main = do
        putStrLn "Count to four for me: "
        putStr "One, two"
        putStr ", three, and"
        putStrLn " four!"
    
    myGreeting :: String
    myGreeting = (++) "hello" " world!"
    
    hello :: String
    hello = "hello"
    
    world :: String
    world = "world!"
    
    mainOne = do
    
        putStrLn myGreeting
    
        putStrLn mySecondGreeting
            where mySecondGreeting = (++) hello ((++) " "  world)
    
    topLevelFunction :: Integer -> Integer
    topLevelFunction x =
        x + woot + topLevelValue
        where woot :: Integer
              woot = 10
    
    topLevelValue :: Integer
    topLevelValue = 5
    
    --Chapter Exercises
    
    addEmphasis :: String -> String
    addEmphasis phrase =
        phrase ++ "!"
    
    fifthCharAsStr :: String -> String
    fifthCharAsStr someString =
        someString !! 4 : []
    
    afterTheNine :: String -> String
    afterTheNine someString =
        drop 9 someString

    thirdChar :: String -> Char
    thirdChar someString =
        someString !! 2
    
    letterIndex :: Int -> String
    letterIndex i = 
       (!!) "Curry is awesome" i : []
    
    rvrse :: String -> String
    rvrse s =
        afterTheNine s ++ " " ++ (!!) s 6 : [] ++ (!!) s 7 : [] ++ " " ++ take 5 s
         