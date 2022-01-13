{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y = (x*x) + (y*y)

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}


minmax :: Int -> Int -> Int -> Int
minmax x y z = max a $ max b c
    where a = abs (x - y)
          b = abs (x - z)
          c = abs (z - y)

{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}


subString :: Int -> Int -> String -> String
subString start end = fromNtakeM start0 (end0-start0)
    where start0 = max start 0
          end0 = max (end+1) 0
          fromNtakeM :: Int -> Int-> String -> String
          fromNtakeM n m = take m . drop n



{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Int
strSum str = sum $ map read $ words str

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greated than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}

lowerAndGreater :: Int -> [Int] -> [Char]
lowerAndGreater number arr = printOut number greaterNum lowerNum
    where lowerAndGreaterArr :: Int -> [Int] -> [Int]
          lowerAndGreaterArr _ []     = []
          lowerAndGreaterArr n [x]    | x > n = [1] | x < n = [-1] | otherwise = [0]
          lowerAndGreaterArr n (x:xs) = lowerAndGreaterArr n [x]  ++ lowerAndGreaterArr n xs

          numTimesFound :: Eq a => a -> [a] -> Int
          numTimesFound x = length . filter (== x)

          lowerNum = numTimesFound 1 $ lowerAndGreaterArr number arr
          greaterNum = numTimesFound (-1) $ lowerAndGreaterArr number arr

          printOut :: Int -> Int -> Int -> String
          printOut n gn ln = show n ++ " is greater than " ++ show gn ++ " elements and lower than " ++ show ln ++ " elements"
