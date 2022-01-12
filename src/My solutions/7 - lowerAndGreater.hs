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