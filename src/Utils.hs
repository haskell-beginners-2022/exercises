module Utils
    ( count
    , count2
    , (!?)
    , mapEither
    , mapRight
    , mapRight2
    , lookupAll
    , safeSum
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

infixl 9 !?
-- As per https://hackage.haskell.org/package/extra-1.7.10/docs/src/Data.List.Extra.html#%21%3F
{- | Like @!!@, but wraps the result in a @Maybe@ and returns @Nothing@ if there is no element at that position
-}
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
xs !? n = foldr (\x r k -> case k of
    0 -> Just x
    _ -> r (k - 1)) (const Nothing) xs n


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

-- As per https://hackage.haskell.org/package/safe-decimal-0.2.1.0/docs/src/Numeric.Decimal.BoundedArithmetic.html#plusBounded
{- | Checks for over-/under-flow before summing
-}
safeSum :: (Ord a, Num a, Bounded a) => a -> a -> Maybe a
safeSum a b
    | sameSign && signA == 1 && a > maxBound - b = Nothing
    | sameSign && signA == -1 && a > minBound - b = Nothing
    | otherwise = Just $ a + b
    where
        signA = signum a
        signB = signum b
        sameSign = signA == signB
