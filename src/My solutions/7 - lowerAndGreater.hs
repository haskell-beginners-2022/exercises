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
lowerAndGreater :: Integer -> [Integer] -> [Char]
lowerAndGreater n list = show
    n
        ++ " is greater than "
        ++ show smaller
        ++ " elements and lower than "
        ++ show larger
        ++ " elements"
    where
        smaller = length $ filter (<n) list
        larger = length $ filter (>n) list

-------

lowerAndGreater' :: Integer -> [Integer] -> [Char]
lowerAndGreater' n list = show
    n
        ++ " is greater than "
        ++ show smaller
        ++ " elements and lower than "
        ++ show larger
        ++ " elements"
    where
        smaller = length $ takeWhile (<n) list
        larger = length $ dropWhile (<n + 1) list 

-----------

lowerAndGreater'' :: Integer -> [Integer] -> [Char]
lowerAndGreater'' n list = show
    n
        ++ " is greater than "
        ++ show smaller
        ++ " elements and lower than "
        ++ show larger
        ++ " elements"
    where
        split = span (<n) list
        smaller = fst split
        larger = snd split


-- Нужно решить через span, takeWhile и свертку с двойным условием. 
-- Осмотреть тесты и понять какие решения не подходят и почему.