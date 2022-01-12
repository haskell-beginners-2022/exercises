{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}

lastDigit :: Integer  -> Integer
lastDigit 0 = 0
lastDigit n = abs n `mod` 10

-----

lastDigit' :: Integer  -> Integer
lastDigit' n
    | n == 0 = 0
    | n > 0 = n `mod` 10
    | otherwise = (-n) `mod` 10