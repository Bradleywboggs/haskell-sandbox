--hello_haskell.hs

module Hello 
     ( triple )
      where
sayHello :: String -> IO ()

sayHello x =
    putStrLn("Hello," ++x++ "!")

half x = x/2

triple x = x * 3

quad x = x * 4

square x = x * x

areaOfCircle r = pi * (square r)

