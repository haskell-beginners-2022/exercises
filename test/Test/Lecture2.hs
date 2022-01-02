module Test.Lecture2
    ( lecture2Spec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Lecture2


lecture2Spec :: Spec
lecture2Spec = describe "Lecture 2" $ do
    describe "lazyProduct" $ do
        it "0 on []" $ lazyProduct [] `shouldBe` 0
        it "x on [x]" $ lazyProduct [10] `shouldBe` 10
