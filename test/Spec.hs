module Main (main) where

import Exercises (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
