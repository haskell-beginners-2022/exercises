module Utils
    ( count
    , (!?)
    ) where

{- | GHC 9 comes with @GHC.Utils.Misc.count@, this is GHC 8 tho
-}
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count p (x : xs) | p x = count p xs + 1
count p (_ : xs) = count p xs

infixl 9 !?
-- As per https://hackage.haskell.org/package/extra-1.7.10/docs/src/Data.List.Extra.html#%21%3F
{- | Like @!!@, but wraps the result in a @Maybe@ and returns @Nothing@ if there is no element at that position
-}
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
xs !? n = foldr (\x r k -> case k of
    0 -> Just x
    _ -> r (k - 1)) (const Nothing) xs n