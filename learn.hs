--learn.hs

module Learn where

import Hello

x = 10 * 5 + y

myResult = x * 5

y = 10

printInc n = print plusTwo
    where plusTwo = n + 2


printCubeVol s = print lwh
    where lwh = s ^ 3


opOne     = x * 3 + y
    where x = 3
          y = 1000


opTwo       = x * 5
    where y = 1000
          x = 10 * 5 + y


opThree     = z / x + y
    where x = 7
          y = negate x
          z = y * 10


waxOn       = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

waxOff x = ((triple x)^2) * 4