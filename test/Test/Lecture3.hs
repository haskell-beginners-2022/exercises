{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Lecture3
    ( lecture3Spec
    ) where

import Data.List (permutations, sort)
import GHC.Stack (HasCallStack)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (assert, forAll, hedgehog, (===))

import Lecture3 (Weekday (..), toShortString)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


lecture3Spec :: Spec
lecture3Spec = describe "Lecture 3" $ do
    describe "toShortString" $ do
        it "Monday" $ toShortString Monday `shouldBe` "Mon"
