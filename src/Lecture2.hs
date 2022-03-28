{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding) where

-- VVV If you need to import libraries, do it after this line ... VVV
-- VVV If you need to import libraries, do it after this line ... VVV
import Data.Either ( fromRight, isLeft )
import Data.Char (isSpace)
-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct = prod 1
  where
    prod :: Int -> [Int] -> Int
    prod acc [] = acc
    prod acc (x: xs)
      | x == 0 = 0
      | otherwise = prod (acc * x) xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt n (x:xs)
  | n < 0 = (Nothing , x:xs)
  | n == 0 = (Just x, xs)
  | otherwise = mergeResultTuple (Nothing , [x]) (removeAt (n -1) xs)

mergeResultTuple :: (Maybe a, [a]) -> (Maybe a, [a]) -> (Maybe a, [a])
mergeResultTuple (Nothing, l1) (Just a, l2) = (Just a, l1 ++ l2)
mergeResultTuple (Nothing, l1) (Nothing, l2) = (Nothing, l1 ++ l2)

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists [] = []
evenLists [[]] = [[]]
evenLists (x:xs) = filterEven x ++ evenLists xs

filterEven :: [a] -> [[a]]
filterEven [] = [[]]
filterEven l
    | even (length l) = [l]
    | otherwise  = []

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = filter (not.isSpace).take 1024

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth    :: Int
    , knightAttack    :: Int
    , knightEndurance :: Int
    }
    deriving (Show)

data Dragon = Dragon
    { dragonHealth     :: Int
    , dragonExpReward :: Int
    , dragonFirePower  :: Int
    }
    deriving (Show)

--type GreenDragon = Dragon
--type RedDragon   = Dragon
--type BlackDragon = Dragon

data Chest a = Chest
    { chestGold     :: Int
    , chestTreasure :: a
    }
    deriving (Show)

data Armor = Armo
    { armorDefence :: Int
    , armorCost    :: Int
    }
    deriving (Show)

data Sword = Sword
    { swordImpact :: Int
    , swordCost   :: Int
    }
    deriving (Show)

data Artifact = Artifact
    { artifactBoost :: Int
    , artifactCost  :: Int
    }
    deriving (Show)

data Gemstone = Gemstone
    {
      gemstoneCost :: Int
    }
    deriving (Show)

type BronzeChest = Chest Armor
type SilverChest = Chest (Sword, Armor)
type GoldenChest = Chest (Artifact, Sword, [Gemstone])


data RewardChest
  = Bronze BronzeChest
  | Silver SilverChest
  | Golden GoldenChest
  deriving (Show)

data FightResult
  = KnightWin
  | DragonWin
  | KnightRunAway
  deriving (Show)

dragonFight :: Dragon -> Knight -> FightResult
dragonFight = turn 1
  where
  turn :: Int -> Dragon -> Knight -> FightResult
  turn cnt dragon knight
    | dragonHealth    dragon <= 0 = KnightWin
    | knightHealth    knight <= 0 = DragonWin
    | knightEndurance knight == 0 = KnightRunAway
    | otherwise =
      if mod cnt 10 == 0
      then turn (cnt + 1) (dragonHit dragon (knightAttack knight)) (khightHit knight (dragonFirePower dragon))
      else turn (cnt + 1) (dragonHit dragon (knightAttack knight)) (khightHit knight 0)

dragonHit :: Dragon -> Int -> Dragon
dragonHit d atk =
  Dragon { dragonHealth = dragonHealth d - atk
  , dragonExpReward = dragonExpReward d
  , dragonFirePower = dragonFirePower d }

khightHit :: Knight -> Int -> Knight
khightHit k atk =
  Knight {knightAttack = knightAttack k
  , knightEndurance = knightEndurance k - 1
  , knightHealth = knightHealth k - atk}

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing (x:xs) = isLess x xs
  where
  isLess :: Int -> [Int] -> Bool
  isLess _ [] = True
  isLess n (x: xs)
    | n > x = False
    | otherwise = isLess x xs


{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] l = l
merge l [] = l
merge (x1: xs1) (x2: xs2)
  | x1 > x2 = x2 : merge (x1: xs1) xs2
  | x1 < x2 = x1 : merge xs1 (x2: xs2)
  | otherwise = [x1, x1] ++ merge xs1 xs2

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort l
      | length l < 2 = l
      | otherwise =
        let
          sl = splitAt (div (length l) 2) l
        in merge (mergeSort (fst sl)) (mergeSort (snd sl))



{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
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

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit n) = Right n
eval l (Add a b) =
  let
   expRes1 = eval l a
   expRes2 = eval l b
  in sumExpRes expRes1 expRes2
eval l (Var s) =
  let
    varVal = lookup s l
  in
    maybe (Left (VariableNotFound s)) Right varVal

sumExpRes :: Either EvalError Int -> Either EvalError Int -> Either EvalError Int
sumExpRes expRes1 expRes2
    | isLeft expRes1 = expRes1
    | isLeft expRes2 = expRes2
    | otherwise = Right (fromRight 0 expRes1 + fromRight 0 expRes2)

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
constantFolding expr =
  let
    decomposeExpr = exprDecompose expr
  in uncurry exprCompose decomposeExpr

exprDecompose :: Expr -> ([Expr], [Expr])
exprDecompose (Lit n) = ([], [Lit n])
exprDecompose (Var v) = ([Var v], [])
exprDecompose (Add x xs)
  | isVar x && isVar xs = ([x, xs], [])
  | isVar x && isLit xs = ([x], [xs])
  | isLit x && isVar xs = ([xs], [x])
  | isLit x && isLit xs = ([], [x, xs])
  | isAdd x && isLit xs = mergeDecmpExprTuple ([],[xs]) (exprDecompose x)
  | isAdd x && isVar xs = mergeDecmpExprTuple ([xs], []) (exprDecompose x)
  | isAdd x && isAdd xs = mergeDecmpExprTuple (exprDecompose x) (exprDecompose xs)
  | otherwise = ([],[])

mergeDecmpExprTuple ::  ([Expr], [Expr]) -> ([Expr], [Expr]) -> ([Expr], [Expr])
mergeDecmpExprTuple (x, xs) (y, ys) = (mergeExprList x y, mergeExprList xs ys)

mergeExprList :: [Expr] -> [Expr] -> [Expr]
mergeExprList [] ys = ys
mergeExprList (x:xs) ys = x:mergeExprList ys xs

exprCompose :: [Expr] -> [Expr] -> Expr
exprCompose [] [] = Lit 0
exprCompose (x:xs) [] = sumVarExpr (x:xs)
exprCompose [] (x:xs) = sumLitExpr (x:xs)
exprCompose (x:xs) (y:ys) =
  let
    sumVar = sumVarExpr (x:xs)
    sumLit = sumLitExpr (y:ys)
  in sumVarSumLitExpr sumVar sumLit

sumVarExpr :: [Expr] -> Expr
sumVarExpr (x:xs)
  | null xs = x
  | otherwise = Add (sumVarExpr xs) x

sumLitExpr :: [Expr] -> Expr
sumLitExpr (x:xs)
  | null xs = x
  | otherwise = sumLitExpr' (getIntFromLit x) xs

sumLitExpr' :: Int -> [Expr] -> Expr
sumLitExpr' n (x:xs)
  | null xs = Lit (n + getIntFromLit x)
  | otherwise = sumLitExpr' (getIntFromLit x) xs

sumVarSumLitExpr :: Expr -> Expr -> Expr
sumVarSumLitExpr v l
  | getIntFromLit l == 0 = v
  | otherwise = Add v l

isAdd :: Expr -> Bool
isAdd (Add _ _) = True
isAdd _         = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _       = False

isLit :: Expr -> Bool
isLit (Lit _) = True
isLit _       = False

getIntFromLit :: Expr -> Int
getIntFromLit (Lit n) = n