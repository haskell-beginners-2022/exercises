{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares x y = sum . map (^2) $ [x, y]

----

sumOfSquares'' :: Integer -> Integer -> Integer
sumOfSquares'' x y = x * x + y * y