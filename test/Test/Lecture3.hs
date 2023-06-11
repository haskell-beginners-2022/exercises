{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Lecture3
    ( lecture3Spec
    ) where

import Hedgehog (Gen)
import Hedgehog.Classes (foldableLaws, functorLaws, lawsCheck, monoidLaws, semigroupLaws)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Lecture3 (Gold (..), List1 (..), Reward (..), Treasure (..), Weekday (..), appendDiff3,
                 daysTo, next, toShortString)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


lecture3Spec :: Spec
lecture3Spec = describe "Lecture 3" $ do
    describe "toShortString" $ do
        it "Monday"    $ toShortString Monday    `shouldBe` "Mon"
        it "Tuesday"   $ toShortString Tuesday   `shouldBe` "Tue"
        it "Wednesday" $ toShortString Wednesday `shouldBe` "Wed"
        it "Thursday"  $ toShortString Thursday  `shouldBe` "Thu"
        it "Friday"    $ toShortString Friday    `shouldBe` "Fri"
        it "Saturday"  $ toShortString Saturday  `shouldBe` "Sat"
        it "Sunday"    $ toShortString Sunday    `shouldBe` "Sun"

    describe "next" $ do
        it "Monday"    $ next Monday    `shouldBe` Tuesday
        it "Tuesday"   $ next Tuesday   `shouldBe` Wednesday
        it "Wednesday" $ next Wednesday `shouldBe` Thursday
        it "Thursday"  $ next Thursday  `shouldBe` Friday
        it "Friday"    $ next Friday    `shouldBe` Saturday
        it "Saturday"  $ next Saturday  `shouldBe` Sunday
        it "Sunday"    $ next Sunday    `shouldBe` Monday

    describe "daysTo" $ do
        it "Monday -> Friday" $ daysTo Monday Friday `shouldBe` 4
        it "Monday -> Sunday" $ daysTo Monday Sunday `shouldBe` 6
        it "Sunday -> Monday" $ daysTo Sunday Monday `shouldBe` 1
        it "Sunday -> Friday" $ daysTo Sunday Friday `shouldBe` 5

    describe "Gold" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genGold) `shouldReturn` True
        it "Laws: Monoid" $
            lawsCheck (monoidLaws genGold) `shouldReturn` True

    describe "Reward" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genReward) `shouldReturn` True
        it "Laws: Monoid" $
            lawsCheck (monoidLaws genReward) `shouldReturn` True

    describe "List1" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genList1) `shouldReturn` True
        it "Includes first element of second list" $
            List1 1  [2,3] <> List1 4 [5, 6] `shouldBe` List1 1 [2, 3, 4, 5, 6]

    describe "Treasure" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genTreasure) `shouldReturn` True
        it "Laws: Monoid" $
            lawsCheck (monoidLaws genTreasure) `shouldReturn` True

    describe "appendDiff3" $ do
        it "x y z" $ appendDiff3 [1] [2] [3] `shouldBe` [1, 2, 3]
        it "x y y" $ appendDiff3 [1] [2] [2] `shouldBe` [1, 2]
        it "y y z" $ appendDiff3 [2] [2] [3] `shouldBe` [2, 3]
        it "x z z" $ appendDiff3 [1] [3] [3] `shouldBe` [1, 3]
        it "z y z" $ appendDiff3 [3] [2] [3] `shouldBe` [3, 2]
        it "x x z" $ appendDiff3 [1] [1] [3] `shouldBe` [1, 3]
        it "x y x" $ appendDiff3 [1] [2] [1] `shouldBe` [1, 2]
        it "x x x" $ appendDiff3 [1] [1] [1] `shouldBe` [1]
        it "Checks duplicates only for original values" $
            appendDiff3 [1] [2] [1, 2] `shouldBe` [1, 2, 1, 2]

    describe "Laws: Foldable" $ do
        it "List1" $ do
            lawsCheck (foldableLaws genList1With) `shouldReturn` True
        it "Treasure" $ do
            lawsCheck (foldableLaws genTreasureWith) `shouldReturn` True

    describe "Laws: Functor" $ do
        it "List1" $ do
            lawsCheck (functorLaws genList1With) `shouldReturn` True
        it "Treasure" $ do
            lawsCheck (functorLaws genTreasureWith) `shouldReturn` True

genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear 0 10)

genGold :: Gen Gold
genGold = Gold <$> genSmallInt

genReward :: Gen Reward
genReward = Reward <$> genGold <*> Gen.bool

genList1 :: Gen (List1 Int)
genList1 = genList1With genSmallInt

genTreasure :: Gen (Treasure Gold)
genTreasure = genTreasureWith genGold

genList1With :: Gen a -> Gen (List1 a)
genList1With gen = List1 <$> gen <*> Gen.list (Range.linear 0 10) gen

genTreasureWith :: Gen a -> Gen (Treasure a)
genTreasureWith gen = Gen.frequency
    [ (1, pure NoTreasure)
    , (3, SomeTreasure <$> gen)
    ]
