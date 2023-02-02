-- |
-- Module                  : Lecture2
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 2 of the Haskell Beginners course.
--
-- As in the previous section, implement functions and provide type
-- signatures. If the type signature is not already written, write the
-- most polymorphic type signature you can.
--
-- Unlike exercises to Lecture 1, this module also contains more
-- challenging exercises. You don't need to solve them to finish the
-- course but you can if you like challenges :)
module Lecture2
  ( -- * Normal
    lazyProduct,
    duplicate,
    removeAt,
    evenLists,
    dropSpaces,
    Knight (..),
    dragonFight,

    -- * Hard
    isIncreasing,
    merge,
    mergeSort,
    Expr (..),
    Variables,
    EvalError (..),
    eval,
    constantFolding,
  )
where

-- VVV If you need to import libraries, do it after this line ... VVV

import Data.Char (isSpace)

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Implement a function that finds a product of all the numbers in
-- the list. But implement a lazier version of this function: if you see
-- zero, you can stop calculating product and return 0 immediately.
--
-- >>> lazyProduct [4, 3, 7]
-- 84
lazyProduct :: [Int] -> Int
lazyProduct lst = case lst of
  [] -> 1
  (0 : _) -> 0
  [x] -> x
  (x : xs) -> x * lazyProduct xs

-- | Implement a function that duplicates every element in the list.
--
-- >>> duplicate [3, 1, 2]
-- [3,3,1,1,2,2]
-- >>> duplicate "cab"
-- "ccaabb"
duplicate :: [a] -> [a]
duplicate lst = case lst of
  [] -> []
  (x : xs) -> x : x : duplicate xs

-- | Implement function that takes index and a list and removes the
-- element at the given position. Additionally, this function should also
-- return the removed element.
--
-- >>> removeAt 0 [1 .. 5]
-- (Just 1,[2,3,4,5])
--
-- >>> removeAt 10 [1 .. 5]
-- (Nothing,[1,2,3,4,5])
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt = removeHelper []
  where
    -- cannot use length, lst can be infinite
    removeHelper :: [a] -> Int -> [a] -> (Maybe a, [a])
    removeHelper before index after
      | index < 0 = (Nothing, after)
      | null after = (Nothing, before)
      | length before == index = (Just (head after), before ++ drop 1 after)
      | otherwise = removeHelper (before ++ take 1 after) index (drop 1 after)

-- | Write a function that takes a list of lists and returns only
-- lists of even lengths.
--
-- >>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
-- [[3,1,2,7],[]]
--
-- ♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
--  in this function.
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

-- | The @dropSpaces@ function takes a string containing a single word
-- or number surrounded by spaces and removes all leading and trailing
-- spaces.
--
-- >>> dropSpaces "   hello  "
-- "hello"
-- >>> dropSpaces "-200            "
-- "-200"
--
-- ♫ NOTE: As in the previous task, use eta-reduction and function
--  composition (the dot (.) operator) in this function.
--
-- 🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
dropSpacesBefore :: String -> String
dropSpacesBefore str
  | null str = str
  | isSpace (head str) = dropSpacesBefore (tail str)
  | otherwise = str

dropSpacesAfter :: String -> String
dropSpacesAfter str
  | null str = str
  | isSpace (head str) = ""
  | otherwise = head str : dropSpacesAfter (tail str)

dropSpaces :: String -> String
dropSpaces = dropSpacesAfter . dropSpacesBefore

-- the str can be infinite, cannot filter each char
-- dropSpaces = filter (not . isSpace)

-- |
--
-- The next task requires to create several data types and functions to
-- model the given situation.
--
-- An evil dragon attacked a village of innocent citizens! After
-- returning to its lair, the dragon became hungry and ate one of its
-- treasure chests by accident.
--
-- The guild in the village found a brave knight to slay the dragon!
-- As a reward, the knight can take the treasure chest.
--
-- Below is the description of the fight and character specifications:
--
--   * A chest contains a non-zero amount of gold and a possible treasure.
--     When defining the type of a treasure chest, you don't know what
--     treasures it stores inside, so your chest data type must be able
--     to contain any possible treasure.
--   * As a reward, the knight takes all the gold, the treasure and experience.
--   * Experience is calculated based on the dragon type. A dragon can be
--     either red, black or green.
--   * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
--   * Stomachs of green dragons contain extreme acid and they melt any
--     treasure except gold. So green dragons have only gold as reward.
--     All other dragons always contain treasure in addition to gold.
--   * Knight tries to slay a dragon with their sword. Each sword strike
--     decreases dragon health by the "sword attack" amount. When the
--     dragon health becomes zero or less, a dragon dies and the knight
--     takes the reward.
--   * After each 10 sword strikes, the dragon breathes fire and decreases
--     knight health by the amount of "dragon fire power". If the
--     knight's health becomes 0 or less, the knight dies.
--   * Additionally, each sword strike decreases "knight's endurance" by one.
--     If a knight's endurance becomes zero, they become tired and are not
--     able to continue the fight so they run away.
--
-- Implement data types to describe treasure, knight and dragon.
-- And implement a function that takes a knight and a dragon and returns
-- one of the three possible fight outcomes.
--
-- You're free to define any helper functions.
--
-- 🕯 HINT: If you find the description overwhelming to implement entirely
--   from scratch, try modelling the problem in stages.
--
--     1. Implement all custom data types without using polymorphism.
--     2. Add @newtype@s for safety where you think is appropriate.
--     3. Encode the fight result as a sum type.
--     4. Add polymorphism.
--     5. Make all invalid states unrepresentable. Think, how you can
--        change your types to prevent green dragons from having any
--        treasure besides gold (if you already haven't done this).

-- some help in the beginning ;)
data Knight = Knight
  { knightHealth :: Int,
    knightAttack :: Int,
    knightEndurance :: Int,
    knightExperience :: Int
    -- knightReward is polymorphic?
  }
  deriving (Show, Eq)

data Chest a = Chest
  { chestGold :: Int,
    chestTreasure :: Maybe a
  }
  deriving (Show, Eq)

data DragonColor
  = Red
  | Black
  | Green
  deriving (Show, Eq)

data Dragon = Dragon
  { dragonColor :: DragonColor,
    dragonHealth :: Int,
    dragonAttack :: Int
  }
  deriving (Show, Eq)

getExperience :: Dragon -> Int
getExperience dragon = case dragonColor dragon of
  Red -> 100
  Black -> 150
  Green -> 250

data FightState = FightState
  { knightState :: Knight,
    dragonState :: Dragon
  }
  deriving (Show, Eq)

data FightResult
  = KnightWin FightState
  | KnightDie FightState
  | KnightRun FightState
  deriving (Show, Eq)

knightTurn :: Knight -> Dragon -> FightState
knightTurn knight dragon =
  FightState
    { knightState = knight {knightEndurance = knightEndurance knight - 1},
      dragonState = dragon {dragonHealth = dragonHealth dragon - knightAttack knight}
    }

dragonTurn :: Knight -> Dragon -> FightState
dragonTurn knight dragon =
  FightState
    { knightState = knight {knightHealth = knightHealth knight - dragonAttack dragon},
      dragonState = dragon
    }

fightTakeTurns :: Int -> FightState -> FightResult
fightTakeTurns turnsCount state
  | knightHealth knight <= 0 = KnightDie state
  | knightEndurance knight <= 0 = KnightRun state
  | dragonHealth dragon <= 0 = KnightWin FightState {knightState = knight {knightExperience = getExperience dragon}, dragonState = dragon}
  | turnsCount /= 0 && mod turnsCount 10 == 0 = fightTakeTurns (turnsCount + 1) (dragonTurn knight dragon)
  | otherwise = fightTakeTurns (turnsCount + 1) (knightTurn knight dragon)
  where
    knight = knightState state
    dragon = dragonState state

dragonFight :: Knight -> Dragon -> FightResult
dragonFight knight dragon = fightTakeTurns 0 initialstate
  where
    initialstate = FightState {knightState = knight, dragonState = dragon}

-- test dragonFight
initKnight = Knight {knightAttack = 15, knightEndurance = 30, knightHealth = 100, knightExperience = 10}

initDragon = Dragon {dragonAttack = 30, dragonHealth = 250, dragonColor = Red}

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

-- | Write a function that takes a list of numbers and returns 'True'
-- if all the numbers are in the increasing order (i.e. the list is
-- sorted).
--
-- >>> isIncreasing [3, 1, 2]
-- False
-- >>> isIncreasing [1 .. 10]
-- True
isIncreasing :: [Int] -> Bool
isIncreasing lst = case lst of
  [] -> True
  [_] -> True
  (x : xs) -> (x < head xs) && isIncreasing xs

-- | Implement a function that takes two lists, sorted in the
-- increasing order, and merges them into new list, also sorted in the
-- increasing order.
--
-- The lists are guaranteed to be given sorted, so you don't need to
-- verify that.
--
-- >>> merge [1, 2, 4] [3, 7]
-- [1,2,3,4,7]
merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge (xa : xsa) (xb : xsb)
  | xa < xb = xa : merge xsa (xb : xsb)
  | otherwise = xb : merge (xa : xsa) xsb

-- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
-- function takes a list of numbers and returns a new list containing the
-- same numbers but in the increasing order.
--
-- The algorithm of merge sort is the following:
--
--  1. If the given list has less than 2 elements, it's already sorted.
--  2. Otherwise, split list into two lists of the same size.
--  3. Sort each of two lists recursively.
--  4. Merge two resulting sorted lists to get a new sorted list.
--
-- >>> mergeSort [3, 1, 2]
-- [1,2,3]
mergeSort :: [Int] -> [Int]
mergeSort lst = case lst of
  [] -> []
  [x] -> [x]
  _ -> merge (mergeSort (take half lst)) (mergeSort (drop half lst))
    where
      half = div (length lst) 2

-- | Haskell is famous for being a superb language for implementing
-- compilers and interpreters to other programming languages. In the next
-- tasks, you need to implement a tiny part of a compiler.
--
-- We're going to work on a small subset of arithmetic operations.
--
-- In programming we write expressions like "x + 1" or "y + x + 10".
-- Such expressions can be represented in a more structured way (than a
-- string) using the following recursive Algebraic Data Type:
data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

-- | We want to evaluate such expressions. We can associate a value
-- with a variable using a list of pairs.
--
-- You can use the @lookup@ function to search in this list by a variable name:
--
-- * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
type Variables = [(String, Int)]

-- | Unfortunately, it's not guaranteed that variables in our @Expr@
-- data type are present in the given list. So we're going to introduce a
-- separate data for possible evaluation errors.
--
-- Normally, this would be a sum type with several constructors
-- describing all possible errors. But we have only one error in our
-- evaluation process.
data EvalError
  = VariableNotFound String
  deriving (Show, Eq)

type EvalResult = Either EvalError Int

-- | Having all this set up, we can finally implement an evaluation function.
-- It returns either a successful evaluation result or an error.
eval :: Variables -> Expr -> Either EvalError Int
eval vars expr = case expr of
  Lit cons -> Right cons
  Var var -> case lookup var vars of
    Nothing -> Left (VariableNotFound var)
    Just val -> Right val
  Add ea eb -> evalAdd (eval vars ea) (eval vars eb)
    where
      evalAdd :: EvalResult -> EvalResult -> EvalResult
      evalAdd (Left err) _ = Left err
      evalAdd _ (Left err) = Left err
      evalAdd (Right ra) (Right rb) = Right (ra + rb)

-- test eval
-- x + y + 10
-- > eval vs e
-- Right 45
-- e :: Expr
-- e = Add (Var "y") (Add (Var "x") (Lit 10))
-- vs :: Variables
-- vs = [("x", 15), ("y", 20)]

-- | Compilers also perform optimizations! One of the most common
-- optimizations is "Constant Folding". It performs arithmetic operations
-- on all constants known during compile time. This way you can write
-- more verbose and clear code that works as efficient as its shorter
-- version.
--
-- For example, if you have an expression:
--
-- x + 10 + y + 15 + 20
--
-- The result of constant folding can be:
--
-- x + y + 45
--
-- It also can be:
--
-- x + 45 + y
--
-- Write a function that takes and expression and performs "Constant
-- Folding" optimization on the given expression.

-- If the given expr is Var or Lit, it's already split
-- Otherwise, split each argument expr of Add recursively
-- Merge two resulting folded splits to get a new split expr

type SplitResult = ([String], Int)

constantSplit :: Expr -> SplitResult
constantSplit expr = case expr of
  Lit c -> ([], c)
  Var v -> ([v], 0)
  Add ea eb -> mergeSplit (constantSplit ea) (constantSplit eb)
    where
      mergeSplit :: SplitResult -> SplitResult -> SplitResult
      mergeSplit (vas, ca) (vbs, cb) = (vas ++ vbs, ca + cb)

assembleSplit :: SplitResult -> Expr
assembleSplit ([], lit) = Lit lit
assembleSplit ([v], 0) = Var v
assembleSplit ([v], lit) = Add (Var v) (Lit lit)
assembleSplit (v : vs, lit) = Add (Var v) (assembleSplit (vs, lit))

constantFolding :: Expr -> Expr
constantFolding = assembleSplit . constantSplit

-- test constantFolding

-- x + 10 + y + 15 + 20
-- e = Add (Var "x") (Add (Lit 10) (Add (Var "y") (Add (Lit 15) (Lit 20))))
