{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS


main :: IO ()
main = do
    LBS.writeFile "test/gen/big.csv"
        $ LBS.concat
        $ take (15 * 10 ^ (7 :: Int))
        $ cycle exampleRows

    putStrLn "Huge file is generated on path: test/gen/big.csv"

exampleRows :: [LBS.ByteString]
exampleRows = map (<> "\n")
    [ "Orange,Buy,10"
    , "Meat,Buy,30"
    , "Strawberry,Buy,100"
    , "Sausage,Sell,100"
    , "Coffee,Sell,50"
    , "Tomatoes,Buy,7"
    , "Wine,Buy,150"
    , "Bag,Sell,1"
    ]
