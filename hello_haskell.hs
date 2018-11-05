sayHello :: String -> IO ()

sayHello x =
    putStrLn("Hello," ++x++ "!")


triple x = x * 3

quad x = x * 4

areaOfCircle r = pi * (r * r)