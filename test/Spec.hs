module Main (main) where

import Test.Hspec (describe, hspec, parallel)

import Test.Lecture1 (lecture1Spec)
import Test.Lecture2 (lecture2Spec)
import Test.Lecture3 (lecture3Spec)


main :: IO ()
main = hspec $ describe "Tests" $ parallel $ do
    lecture1Spec
    lecture2Spec
    lecture3Spec
