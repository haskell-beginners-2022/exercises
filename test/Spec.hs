module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (describe, hspec)

import Test.Lecture1 (lecture1Spec)
import Test.Lecture2 (lecture2Spec)
import Test.Lecture3 (lecture3Spec)


main :: IO ()
main = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec $ describe "Tests" $ do
        lecture1Spec
        lecture2Spec
        lecture3Spec
