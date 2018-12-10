--reverse.hs

module Reverse where

import ChapterThree

main :: IO ()
main = do
    print $ rvrse "Curry is awesome" 

-- rvrse :: String -> String
-- rvrse s = 
--     afterTheNine s ++ " " ++ (!!) s 6 : [] ++ (!!) s 7 : [] ++ 
--     " " ++ take 5 s 


-- afterTheNine :: String -> String
-- afterTheNine someString =
--         drop 9 someString
