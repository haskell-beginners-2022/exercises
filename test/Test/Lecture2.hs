{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Lecture2
    ( lecture2Spec
    ) where

import Data.Bifunctor (second)
import Data.List (permutations, sort)
import GHC.Stack (HasCallStack)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (assert, forAll, hedgehog, (===))

import Lecture2 (EvalError (..), Expr (..), constantFolding, dropSpaces, duplicate, eval, evenLists,
                 isIncreasing, lazyProduct, merge, mergeSort, removeAt)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


lecture2Spec :: Spec
lecture2Spec = describe "Lecture 2" $ do
    lecture2Normal
    lecture2Hard

lecture2Normal :: Spec
lecture2Normal = describe "Normal" $ do
    describe "lazyProduct" $ do
        it "1 on []"        $ lazyProduct []                    `shouldBe` 1
        it "x on [x]"       $ lazyProduct [10]                  `shouldBe` 10
        it "Normal product" $ lazyProduct [3, 1, 2, 7, 5]       `shouldBe` 210
        it "Factorial"      $ lazyProduct [1 .. 10]             `shouldBe` 3628800
        it "Zero when 0"    $ lazyProduct [10, 3, 0, 5]         `shouldBe` 0
        it "Is lazy"        $ lazyProduct [3, 0, error "Oops!"] `shouldBe` 0

        it "lazyProduct ≡ product" $ hedgehog $ do
            xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)
            lazyProduct xs === product xs

    describe "duplicate" $ do
        it "Empty"         $ duplicate ([] :: [Int])     `shouldBe` []
        it "Dup singleton" $ duplicate ([10] :: [Int])   `shouldBe` [10, 10]
        it "Dup same"      $ duplicate ([2, 2] :: [Int]) `shouldBe` [2, 2, 2, 2]
        it "Dup string"    $ duplicate "Hello!"          `shouldBe` "HHeelllloo!!"
        it "Dup infinite"  $ take 5 (duplicate [1 .. ])  `shouldBe` [1, 1, 2, 2, 3]

        it "length (duplicate xs) ≡ 2 * length xs" $ hedgehog $ do
            xs <- forAll $ Gen.list (Range.linear 0 10) Gen.bool
            length (duplicate xs) === 2 * length xs

    describe "removeAt" $ do
        let nothing = Nothing :: Maybe Int
        it "Empty list by zero"   $ removeAt 0 []           `shouldBe` (nothing, [] :: [Int])
        it "Singleton list on 0"  $ removeAt 0 [5]          `shouldBe` (Just 5, [] :: [Int])
        it "Singleton list on 1"  $ removeAt 1 [5]          `shouldBe` (nothing, [5])
        it "Singleton list on -1" $ removeAt (-1) [5]       `shouldBe` (nothing, [5])
        it "List on negative"     $ removeAt (-3) [3, 1, 2] `shouldBe` (nothing, [3, 1, 2])
        it "List on length"       $ removeAt 3 [3, 1, 2]    `shouldBe` (nothing, [3, 1, 2])
        it "List on pre length"   $ removeAt 2 [3, 1, 2]    `shouldBe` (Just 2, [3, 1])
        it "String on middle"     $ removeAt 2 "Hello"      `shouldBe` (Just 'l', "Helo")

        it "Remove from infinite" $ do
            let result = second (take 7) (removeAt 5 [1 .. ])
            result `shouldBe` (Just 6, [1, 2, 3, 4, 5, 7, 8])

        it "Remove from infinite with negative index" $ do
            let result = second (take 5) (removeAt (-1) [1 .. ])
            result `shouldBe` (nothing, [1 .. 5])

    describe "evenLists" $ do
        it "Empty list"      $ evenLists []                    `shouldBe` ([] :: [String])
        it "Singleton empty" $ evenLists [[]]                  `shouldBe` [""]
        it "All odd"         $ evenLists ["foo", "bar", "baz"] `shouldBe` ([] :: [String])
        it "All even"        $ evenLists ["fo", "ba", "ba"] `shouldBe` ["fo", "ba", "ba"]
        it "Mix"             $ evenLists ["foo", "o", "x", "quux"] `shouldBe` ["quux"]
        it "Infinite"        $ take 3 (evenLists (cycle ["zero", "two"])) `shouldBe` ["zero", "zero", "zero"]

    describe "dropSpaces" $ do
        it "Just text"      $ dropSpaces "word"     `shouldBe` "word"
        it "Only leading"   $ dropSpaces "   hi"    `shouldBe` "hi"
        it "Only trailing"  $ dropSpaces "hi   "    `shouldBe` "hi"
        it "Both sides"     $ dropSpaces "   hi   " `shouldBe` "hi"
        it "Single space"   $ dropSpaces " 500 "    `shouldBe` "500"
        it "Infinite space" $ dropSpaces (" infinity" ++ repeat ' ') `shouldBe` "infinity"

lecture2Hard :: Spec
lecture2Hard = describe "Hard" $ do
    describe "isIncreasing" $ do
        it "Empty list" $ isIncreasing [] `shouldBe` True
        it "Singleton list" $ isIncreasing [5] `shouldBe` True
        it "Two elements sorted" $ isIncreasing [1, 2] `shouldBe` True
        it "Two elements chaos" $ isIncreasing [2, 1] `shouldBe` False
        it "Big range" $ isIncreasing [1 .. 1000] `shouldBe` True
        it "First element wrong" $ isIncreasing (100 : [1 .. 5]) `shouldBe` False
        it "Last element wrong" $ isIncreasing ([1 .. 5] ++ [0]) `shouldBe` False
        it "Small, big, small" $ isIncreasing [1, 2, 1] `shouldBe` False
        it "Lazy: doesn't crash" $ isIncreasing [10, 5, error "Oops!"] `shouldBe` False
        it "Lazy: works on infinite lists" $ isIncreasing (10 : [0 .. ]) `shouldBe` False

        it "isIncreasing (sort xs) ≡ True" $ hedgehog $ do
            xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)
            assert $ isIncreasing (sort xs)

    describe "merge" $ do
        it "Two empty" $ merge [] [] `shouldBe` []
        it "Left empty" $ merge [] [1, 2] `shouldBe` [1, 2]
        it "Right empty" $ merge [3, 4] [] `shouldBe` [3, 4]
        it "Two single" $ merge [5] [10] `shouldBe` [5, 10]
        it "Two single, 2" $ merge [10] [5] `shouldBe` [5, 10]
        it "Singleton and range" $ merge [3] [1 .. 5] `shouldBe` [1, 2, 3, 3, 4, 5]
        it "Odd and even" $ merge [0, 2 .. 100] [1, 3 .. 100] `shouldBe` [0 .. 100]
        it "Lazy: two infinite lists" $ take 10 (merge [0, 2 .. ] [1, 3 .. ]) `shouldBe` [0 .. 9]

        it "merge (sort xs) (sort ys) ≡ sort (xs ++ ys)" $ hedgehog $ do
            xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)
            ys <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)
            merge (sort xs) (sort ys) === sort (xs ++ ys)

    describe "mergeSort" $ do
        it "Empty list" $ mergeSort [] `shouldBe` []
        it "Singleton list" $ mergeSort [5] `shouldBe` [5]
        it "Already sorted" $ mergeSort [1 .. 5] `shouldBe` [1 .. 5]
        it "Reversed" $ mergeSort [10, 9 .. 0] `shouldBe` [0 .. 10]
        it "Mix order" $ mergeSort [3, 1, 7, 5, 2] `shouldBe` [1, 2, 3, 5, 7]

        it "mergeSort ≡ sort" $ hedgehog $ do
            xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)
            mergeSort xs === sort xs

    describe "eval" $ do
        it "Lit" $
            eval [] (Lit 5) `shouldBe` Right 5
        it "Var" $
            eval [("x", 42)] (Var "x") `shouldBe` Right 42
        it "Var error" $
            eval [("y", 42)] (Var "x") `shouldBe` Left (VariableNotFound "x")
        it "Add Lit" $
            eval [] (Add (Lit 5) (Lit 10)) `shouldBe` Right 15
        it "Add Var and Lit" $
            eval [("x", 15)] (Add (Var "x") (Lit 1)) `shouldBe` Right 16
        it "Var not found nested" $
            eval [("y", 15)] (Add (Var "x") (Lit 1)) `shouldBe` Left (VariableNotFound "x")
        it "Two vars and nested" $
            eval
                [("x", 15), ("y", 7)]
                (Add (Var "y") (Add (Var "x") (Lit 10)))
              `shouldBe` Right 32

    describe "constantFolding" $ do
        it "Single Var" $ constantFolding (Var "x") `shouldBe` Var "x"
        it "Single Lit" $ constantFolding (Lit 5) `shouldBe` Lit 5
        it "Lit 0" $ constantFolding (Lit 0) `shouldBe` Lit 0
        it "Two lits" $ constantFolding (Add (Lit 5) (Lit 10)) `shouldBe` Lit 15
        it "Var and Lit 0" $ constantFolding (Add (Var "x") (Lit 0)) `shouldBe` Var "x"

        it "Var and sum zero" $
            constantFolding (Add (Add (Lit (-5)) (Var "x")) (Lit 5)) `shouldBe` Var "x"

        it "Var and Lit 5" $ constantFolding (Add (Var "x") (Lit 5)) `isOneOf`
            [ Add (Var "x") (Lit 5)
            , Add (Lit 5) (Var "x")
            ]

        it "Two vars and two numbers" $
            constantFolding
                ( Add
                    ( Add
                        (Var "x")
                        (Lit 7)
                    )
                    ( Add
                        (Var "y")
                        (Lit 2)
                    )
                )
              `isOneOf` exprPermutations (Var "x") (Var "y") (Lit 9)

isOneOf :: (HasCallStack, Show a, Eq a) => a -> [a] -> Expectation
isOneOf x xs = x `shouldSatisfy` (`elem` xs)

-- Takes 3 expressions and returns a list of all possible valid expressions additions
exprPermutations :: Expr -> Expr -> Expr -> [Expr]
exprPermutations a b c = concatMap tripleAdd $ permutations [a, b, c]
  where
    tripleAdd :: [Expr] -> [Expr]
    tripleAdd [x, y, z] =
        [ Add x (Add y z)
        , Add (Add x y) z
        ]
    tripleAdd _ = error "Expected 3 expressions"
