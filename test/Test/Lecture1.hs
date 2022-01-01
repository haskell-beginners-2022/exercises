module Test.Lecture1
    ( lecture1Spec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Lecture1 (lastDigit, lowerAndGreater, makeSnippet, minmax, strSum, subString, sumOfSquares)


lecture1Spec :: Spec
lecture1Spec = describe "Lecture 1" $ do
    it "Dummy test" $
        True `shouldBe` True
