{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Lecture1
    ( lecture1Spec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Lecture1 (lastDigit, lowerAndGreater, makeSnippet, minmax, strSum, subString, sumOfSquares)


lecture1Spec :: Spec
lecture1Spec = describe "Lecture 1" $ do
    describe "makeSnippet" $
        it "The type is correct" $
            makeSnippet 15 "Lorem ipsum" `shouldBe` "Description: Lo..."

    describe "sumOfSquares" $ do
        it "Zeroes"        $ sumOfSquares 0 0       `shouldBe` 0
        it "Big numbers"   $ sumOfSquares 100 50    `shouldBe` 12500
        it "Both negative" $ sumOfSquares (-8) (-4) `shouldBe` 80

    describe "lastDigit" $ do
        it "Last digit of 0"              $ lastDigit 0       `shouldBe` 0
        it "Last digit of 0 < x < 10"     $ lastDigit 5       `shouldBe` 5
        it "Last digit of 10 < x < 100"   $ lastDigit 34      `shouldBe` 4
        it "Last digit of 100 < x < 1000" $ lastDigit 341     `shouldBe` 1
        it "Last digit of big num"        $ lastDigit 1234789 `shouldBe` 9
        it "Last digit of negative"       $ lastDigit (-12)   `shouldBe` 2

    describe "minmax" $ do
        it "Increasing sequence" $ minmax 1 2 3        `shouldBe` 2
        it "Decreasing sequence" $ minmax 9 8 7        `shouldBe` 2
        it "All three equal"     $ minmax 1 1 1        `shouldBe` 0
        it "Two equal"           $ minmax 0 9 0        `shouldBe` 9
        it "Negative"            $ minmax (-10) 3 (-5) `shouldBe` 13

    describe "subString" $ do
        it "One character"     $ subString 0 0       "Hello!" `shouldBe` "H"
        it "Two characters"    $ subString 0 1       "Hello!" `shouldBe` "He"
        it "Bounds are bigger" $ subString 0 100     "Hello!" `shouldBe` "Hello!"
        it "From negative"     $ subString (-1) 3    "Hello!" `shouldBe` "Hell"
        it "Both negative"     $ subString (-7) (-3) "Hello!" `shouldBe` ""
        it "Negative to zero"  $ subString (-5) 0    "Hello!" `shouldBe` "H"
        it "0 to -1"           $ subString 0 (-1)    "Hello!" `shouldBe` ""
        it "1 to -1"           $ subString 1 (-1)    "Hello!" `shouldBe` ""
        it "Start > 0"         $ subString 2 4       "Hello!" `shouldBe` "llo"

    describe "strSum" $ do
        it "Empty string"           $ strSum ""                   `shouldBe` 0
        it "Single positive number" $ strSum "42"                 `shouldBe` 42
        it "Single negative number" $ strSum "-17"                `shouldBe` (-17)
        it "Spaced number"          $ strSum "  5   "             `shouldBe` 5
        it "Only positive"          $ strSum "  1 2   4   "       `shouldBe` 7
        it "Only negative"          $ strSum "  -1 -2   -4"       `shouldBe` (-7)
        it "Mix"                    $ strSum " 10 -20 30 -40  70" `shouldBe` 50

    describe "lowerAndGreater" $ do
        it "Empty list" $ lowerAndGreater 10 [] `shouldBe`
            "10 is greater than 0 elements and lower than 0 elements"
        it "All elements equal" $ lowerAndGreater 3 [3, 3, 3, 3] `shouldBe`
            "3 is greater than 0 elements and lower than 0 elements"
        it "All elements lower" $ lowerAndGreater 10 [1 .. 5] `shouldBe`
            "10 is greater than 5 elements and lower than 0 elements"
        it "All elements greater" $ lowerAndGreater 10 [11 .. 20] `shouldBe`
            "10 is greater than 0 elements and lower than 10 elements"
        it "Mix and match" $ lowerAndGreater 2 [7, 1, 2, 4, 5, 8, 2, 1, 0] `shouldBe`
            "2 is greater than 3 elements and lower than 4 elements"
