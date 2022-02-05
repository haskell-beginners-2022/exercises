module Test.Lecture4
    ( lecture4Spec
    ) where

import Data.Semigroup (Max (..), Min (..), Sum (..))
import Hedgehog (Gen)
import Hedgehog.Classes (lawsCheck, semigroupLaws)
import System.IO.Silently (silence)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Lecture4 (MaxLen (..), Row (..), Stats (..), TradeType (..), calculateStats, displayStats,
                 parseRow, printProductStats, rowToStats)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


lecture4Spec :: Spec
lecture4Spec = describe "Lecture 4" $ do
    describe "parseRow" $ do
        it "Valid Buy" $ parseRow "Oranges,Buy,10" `shouldBe` Just Row
            { rowProduct   = "Oranges"
            , rowTradeType = Buy
            , rowCost      = 10
            }

        it "Valid Sell" $ parseRow "Apples,Sell,0" `shouldBe` Just Row
            { rowProduct   = "Apples"
            , rowTradeType = Sell
            , rowCost      = 0
            }

        it "Ignores spaces in line" $ parseRow "  Apples  ,  Sell  , 7 " `shouldBe` Just Row
            { rowProduct   = "  Apples  "
            , rowTradeType = Sell
            , rowCost      = 7
            }

        it "Nothing on 2 values"        $ parseRow "unknown,Sell"        `shouldBe` Nothing
        it "Nothing on 4 values"        $ parseRow "unknown,Sell,10,15"  `shouldBe` Nothing
        it "Nothing on invalid type"    $ parseRow "unknown,Exchange,10" `shouldBe` Nothing
        it "Nothing on empty product"   $ parseRow ",Buy,10"             `shouldBe` Nothing
        it "Nothing on non-number"      $ parseRow "unknown,Buy,oranges" `shouldBe` Nothing
        it "Nothing on negative number" $ parseRow "unknown,Buy,-5"      `shouldBe` Nothing
        it "Nothing on double number"   $ parseRow "unknown,Buy,4.2"     `shouldBe` Nothing
        it "Nothing on column names"    $ parseRow "Name,Type,Amount"    `shouldBe` Nothing
        it "Nothing on empty"           $ parseRow ""                    `shouldBe` Nothing
        it "Nothing on 1 commas"        $ parseRow ","                   `shouldBe` Nothing
        it "Nothing on 2 commas"        $ parseRow ",,"                  `shouldBe` Nothing
        it "Nothing on 3 commas"        $ parseRow ",,,"                 `shouldBe` Nothing
        it "Nothing on 9 commas"        $ parseRow ",,,,,,,,,"           `shouldBe` Nothing

    describe "Semigroup: MaxLen" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genMaxLen) `shouldReturn` True
        it "Right side is Larger" $
            MaxLen "12345" <> MaxLen "12" `shouldBe` MaxLen "12345"
        it "Left side is Larger" $
            MaxLen "12" <> MaxLen "12345" `shouldBe` MaxLen "12345"
        it "Both sides equal" $
            MaxLen "abcde" <> MaxLen "12345" `shouldBe` MaxLen "abcde"

    describe "Semigroup: Stats" $ do
        it "Laws: Semigroup" $
            lawsCheck (semigroupLaws genStats) `shouldReturn` True

    describe "rowToStats" $ do
        it "From Buy row" $ rowToStats (Row "Lemon" Buy 15) `shouldBe` Stats
            { statsTotalPositions = Sum 1
            , statsTotalSum       = Sum (-15)
            , statsAbsoluteMax    = Max 15
            , statsAbsoluteMin    = Min 15
            , statsSellMax        = Nothing
            , statsSellMin        = Nothing
            , statsBuyMax         = Just $ Max 15
            , statsBuyMin         = Just $ Min 15
            , statsLongest        = MaxLen "Lemon"
            }

        it "From Sell row" $ rowToStats (Row "Lime" Sell 25) `shouldBe` Stats
            { statsTotalPositions = Sum 1
            , statsTotalSum       = Sum 25
            , statsAbsoluteMax    = Max 25
            , statsAbsoluteMin    = Min 25
            , statsSellMax        = Just $ Max 25
            , statsSellMin        = Just $ Min 25
            , statsBuyMax         = Nothing
            , statsBuyMin         = Nothing
            , statsLongest        = MaxLen "Lime"
            }

    let stringStats =
            [ "Total positions:       : 3"
            , "Total final balance    : -15"
            , "Biggest absolute cost  : 50"
            , "Smallest absolute cost : 10"
            , "Max earning            : 25"
            , "Min earning            : 10"
            , "Max spending           : 50"
            , "Min spending           : 50"
            , "Longest product name   : Pineapples"
            ]

    describe "displayStats" $ do
        let stats = Stats
                { statsTotalPositions = Sum 3
                , statsTotalSum       = Sum (-15)
                , statsAbsoluteMax    = Max 50
                , statsAbsoluteMin    = Min 10
                , statsSellMax        = Just $ Max 25
                , statsSellMin        = Just $ Min 10
                , statsBuyMax         = Just $ Max 50
                , statsBuyMin         = Just $ Min 50
                , statsLongest        = MaxLen "Pineapples"
                }

        it "Documentation example" $ lines (displayStats stats) `shouldBe` stringStats

    describe "calculateStats" $ do
        let input = unlines
                [ "Name,Type,Amount"
                , "Apples,Sell,25"
                , "Tomatoes,Sell,10"
                , "Pineapples,Buy,50"
                ]

        it "Documentation example" $ lines (calculateStats input) `shouldBe` stringStats

    describe "printProductStats" $ do
        it "Doesn't throw exceptions" $
            silence (printProductStats "test/products.csv") `shouldReturn` ()

genMaxLen :: Gen MaxLen
genMaxLen = MaxLen <$> Gen.list (Range.linear 0 20) Gen.alphaNum

genStats :: Gen Stats
genStats = Stats
    <$> (Sum <$> genInt)
    <*> (Sum <$> genInt)
    <*> (Max <$> genInt)
    <*> (Min <$> genInt)
    <*> (Gen.maybe $ Max <$> genInt)
    <*> (Gen.maybe $ Min <$> genInt)
    <*> (Gen.maybe $ Max <$> genInt)
    <*> (Gen.maybe $ Min <$> genInt)
    <*> genMaxLen
  where
    genInt :: Gen Int
    genInt = Gen.int Range.constantBounded
