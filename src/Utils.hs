module Utils
    ( count
    , count2
    , mapEither
    , mapRight
    , mapRight2
    , lookupAll
    , headMaybe
    , tailMaybe
    , listOfTwo
    , listOfThree
    ) where

{- | GHC 9 comes with @GHC.Utils.Misc.count@, this is GHC 8 tho
-}

count :: (Foldable t, Num n) => (a -> Bool) -> t a -> n
count p =
    foldr inner 0
    where
        inner x a' | p x = a' + 1
        inner _ a'       = a'

count2 :: (Foldable t, Num n) => (a -> Bool) -> (a -> Bool) -> t a -> (n, n)
count2 p q =
    foldr inner (0, 0)
    where
        inner x (a', b') | p x && q x = (a' + 1, b' + 1)
        inner x (a', b') | p x        = (a' + 1, b'    )
        inner x (a', b') |        q x = (a'    , b' + 1)
        inner _ (a', b')              = (a', b')

mapEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
mapEither f _ (Left x) = Left $ f x
mapEither _ g (Right y) = Right $ g y

mapRight :: (b -> b') -> Either a b -> Either a b'
mapRight = mapEither id

mapRight2 :: (b -> b' -> b'') -> Either a b -> Either a b' -> Either a b''
mapRight2 _ (Left x) _ = Left x
mapRight2 _ _ (Left y) = Left y
mapRight2 f (Right x) (Right y) = Right $ f x y

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll needle list = map snd $ filter (\x -> fst x == needle) list

-- | there is @listToMaybe@ but it's a stupid name
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_ : xs) = Just xs

listOfTwo :: a -> a -> [a]
listOfTwo a b = [a, b]

listOfThree :: a -> a -> a -> [a]
listOfThree a b c = [a, b, c]