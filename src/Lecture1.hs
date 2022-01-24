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
import Data.Char (digitToInt)
import Utils (count2, (!?))
import Data.Maybe (mapMaybe)

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> String -> String
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
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y = x * x + y * y

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}

lastDigit :: (Enum a, Show a) => a -> a
-- | @mod@? NAAH have this /beautiful/ code
-- Convert @n@ to string, take the last item (via @foldr1@) and convert it back into an int
lastDigit = toEnum . digitToInt . foldr1 (\_ x -> x) . show
-- I guess you wouldn't need a @Show@ instance nor an @Enum@ instance (just an @Integral@ instance) if you used @mod@, but hey :)

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}

minmax :: (Num a, Ord a) => a -> a -> a -> a
minmax x y z = maximum [x, y, z] - minimum [x, y, z]
-- I mean, if you reeeally want me to use where:
{-
minmax x y z =
    max1 - min1
    where
        max1 = max x $ max y z
        min1 = min x $ min y z
-}


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
subString :: Int -> Int -> [a] -> [a]
-- Get all the items at those indexes, drop the ones that don't exist (@mapMaybe@ filters out the @Nothing@s and converts @Just a@s into @a@s)
subString start end str = mapMaybe (str !?) [start..end]

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: (Num a, Read a) => String -> a
-- Split the string into words, @read@ them (type inference does its job here) and @sum@ them up
strSum = sum . map read . words

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
lowerAndGreater :: (Foldable t, Ord a, Show a) => a -> t a -> String
-- I guess @count2@ is implemented recursively, does that count? Hope so, I made it with love 💜
lowerAndGreater n list =
    show n ++ " is greater than " ++ show greater ++ " elements and lower than " ++ show lower ++ " elements"
    where
        lower :: Integer
        greater :: Integer
        (lower, greater) = count2 (> n) (< n) list
