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
    , constantFolding
    ) where

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}

{- It's tail recursive :-) -}
lazyProduct :: [Int] -> Int
lazyProduct lst = go 1 lst
    where
        go :: Int -> [Int] -> Int
        go accu []       = accu
        go accu (0 : xs) = 0
        go accu (x : xs) = go (x * accu) xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
{- Ok, this is really simple -}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x: xs) = x : x : duplicate xs

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt i list
   | i < 0               = (Nothing, list)
   | null list           = (Nothing, list)
   | i == 0              = (Just (head list), tail list)
   | otherwise =
      let x = (removeAt (i-1) (tail list))
         in case x of
            (Nothing, list1)    -> (Nothing, list)
            (Just value, list1) -> (Just value, head list : list1)


{- This is simple, maybe too long -}
{- removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt i list =
   if i < 0
      then
         (Nothing, list)
      else
         let
            l = length list
         in
            if i < l
               then
                 let left =  take i list
                     right = drop i list
                    in
                       (Just(head right), left ++ (tail right))
               else
                 (Nothing, list) -}

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
{-ok, this is quite straightforward -}
evenLists :: [[a]] -> [[a]]
evenLists = removeLenghts . selectEven . computeLenghts
   where
      computeLenghts :: [[a]] -> [([a], Int)]
      computeLenghts list = map (\l -> (l, length l)) list

      selectEven :: [([a], Int)] -> [([a], Int)]
      selectEven list = filter (\x -> (even (snd x))) list

      removeLenghts :: [([a], Int)] -> [[a]]
      removeLenghts list = map (\x -> fst x) list

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

dropSpaces :: [Char] -> [Char]
dropSpaces  = leaveOnlyLetters . dropWhitespacesLeft
   where
      leaveOnlyLetters :: [Char] -> [Char]
      leaveOnlyLetters s
         | null s        = s
         | head s /= ' ' = (head s) : (leaveOnlyLetters (tail s))
         | otherwise     = []

      dropWhitespacesLeft :: [Char] -> [Char]
      dropWhitespacesLeft s
         | null s        = s
         | head s == ' ' = dropWhitespacesLeft (tail s)
         | otherwise     = s

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

{- It's a sum data type, ok, primitive but works -}
data Dragon a =
  RedDragon { dragonGold :: Int, dragonFirepower :: Int, dragonHealth :: Int, dragonTreasure :: a }
  | BlackDragon { dragonGold :: Int, dragonFirepower :: Int, dragonHealth :: Int, dragonTreasure :: a }
  | GreenDragon { dragonGold :: Int, dragonFirepower :: Int, dragonHealth :: Int }

data FightResult a
  = KnightWins { resultExp :: Int, resultGold :: Int, resultTreasure :: Maybe a }
  | KnightLoses
  {- No treasure, no gold, but I guess some exp -}
  | KnightEscapes { resultExp :: Int }

{- instance Show FightResult String where
   show (KnightWins exp gold) = "KnightWins " ++ (show exp) ++ " experience " ++ (show gold) ++ " gold"
   show KnightLoses = "KnightLoses"
   show (KnightEscapes exp) = "KnightEscapes " ++ (show exp) ++ " experience" -}

min3 :: Int -> Int -> Int -> Int
min3 x y z = min (min x y) z

dragonFight :: Dragon a -> Knight -> FightResult a
dragonFight dragon knight =
   let
      {- How many attacks until knight is killed ? -}
      numAttacksToGetKilled = (ceiling (fromIntegral (knightHealth knight)/fromIntegral (dragonFirepower dragon))) * 10

      {- How many attacks until the knight is exhausted ? -}
      numAttacksToBecomeTired = knightEndurance knight

      {- How many attacks until the knight wins ? -}
      numAttacksToWin = ceiling (fromIntegral (dragonHealth dragon)/fromIntegral (knightAttack knight))

      {- How long will the epic fight last ? -}
      numAttacksUntilEnd = min3 numAttacksToGetKilled numAttacksToBecomeTired numAttacksToWin

      knLoses = numAttacksToGetKilled == numAttacksUntilEnd
      knEscapes = numAttacksToBecomeTired == numAttacksUntilEnd
      knWins = numAttacksToWin == numAttacksUntilEnd
      exp = case dragon of
         RedDragon _ _ _ _   -> 100
         BlackDragon _ _ _ _ -> 150
         GreenDragon _ _ _   -> 250
      treasure = case dragon of
         RedDragon _ _ _ t   -> Just t
         BlackDragon _ _ _ t -> Just t
         GreenDragon _ _ _   -> Nothing
   in case () of
    _ | knLoses   -> KnightLoses
      | knEscapes -> KnightEscapes exp
      | knWins    -> KnightWins exp (dragonGold dragon) treasure

{- Overall, the dragon task solution is not very sophisticeted. I'm more happy with compiler stuff :-) -}



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
{- Ok, this is rather straightforward. I wonder where's the catch ? -}
isIncreasing :: [Int] -> Bool
isIncreasing []         = True
isIncreasing [x]        = True
isIncreasing (x : xs)   = x < (head xs) && (isIncreasing xs)

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
{- ok quite easy -}
merge :: [Int] -> [Int] -> [Int]
merge [] bs = bs
merge as [] = as
merge (a : as) (b : bs)
   | a < b     = a : merge as (b:bs)
   | otherwise = b : merge (a:as) (bs)

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
{- ok, no real problems here ! -}
mergeSort :: [Int] -> [Int]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list =
   let l          = length list
       leftLen    = (div l 2)
       leftHalf   = take leftLen list
       rightHalf  = drop leftLen list
   in  merge (mergeSort leftHalf) (mergeSort rightHalf)


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
eval vars expr = case expr of
     Lit value -> Right value
     Var varName ->
        let varValue = lookup varName vars
        in case varValue of
          Nothing    -> Left (VariableNotFound varName)
          Just value -> Right value
     {- I am sure there is more elegant solution, sth like Reactor zip / Mutiny combine -}
     Add expr1 expr2 ->
        let res1 = eval vars expr1
            res2 = eval vars expr2
        in case res1 of
           Right r1      -> fmap (\r2 -> r2 + r1) res2
           Left error    -> Left error

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
{- I think the simplest form possible here is (optionally) a constant, and a list of variables -}
{- So I'm going to traverse the tree, collecting the literal value and vars to get the simplest form -}
constantFolding :: Expr -> Expr
constantFolding expr = case simplifyExpr expr of
   (Nothing, Nothing) -> Lit 0
   (Just v1, Nothing) -> Lit v1
   (Nothing, Just e2) -> e2
   (Just v1, Just e2) -> Add (Lit v1) e2

{- Auxiliary function - processes the tree recursively, bubbling up the constant and isolating the rest of expression-}
simplifyExpr :: Expr -> (Maybe Int, Maybe Expr)
simplifyExpr expr = case expr of
   Lit 0     -> (Nothing, Nothing)
   Lit value -> (Just value, Nothing)
   Var v     -> (Nothing, Just expr)
   Add expr1 expr2 -> let
      res1 = simplifyExpr expr1
      res2 = simplifyExpr expr2
      joinedValues = joinValues (fst res1) (fst res2)
      joinedExpressions = joinExpressions (snd res1) (snd res2)
      in case joinedValues of
        Nothing -> (Nothing, joinedExpressions)
        Just 0  -> (Nothing, joinedExpressions)
        Just v  -> (Just v, joinedExpressions)

joinValues :: Maybe Int -> Maybe Int -> Maybe Int
joinValues = join (\x y -> x+y)

joinExpressions :: Maybe Expr -> Maybe Expr -> Maybe Expr
joinExpressions = join (\x y -> Add x y)

{- An approach to construct a "linker" of two Maybes -}
join :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
join link m1 m2 = case (m1, m2) of
   (Nothing, Nothing) -> Nothing
   (Just m1, Nothing) -> Just m1
   (Nothing, Just m2) -> Just m2
   (Just m1, Just m2) -> Just (link m1 m2)

{- It was the first approach: joinValues and joinExpressions functions. It works, but they are too similar to leave them like that-}
{-joinValues v1 v2 = case (v1, v2) of
   (Nothing, Nothing) -> Nothing
   (Just v1, Nothing) -> Just v1
   (Nothing, Just v2) -> Just v2
   (Just v1, Just v2) -> Just (v1 + v2) -}

{- joinExpressions e1 e2 = case (e1, e2) of
   (Nothing, Nothing) -> Nothing
   (Just e1, Nothing) -> Just e1
   (Nothing, Just e2) -> Just e2
   (Just e1, Just e2) -> Just (Add e1 e2) -}
