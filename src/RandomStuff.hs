{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module RandomStuff where
import Text.Printf (PrintfArg (formatArg))

{-
hello reader
these are the random things I made in haskell that deserve to be forever archived
-}

{- | multiply, but without @(*)@
-}
mult :: Int -> Int -> Int
mult a b =
    if shouldBeNegative then -absResult else absResult
    where
        zipWithIndex :: [a] -> [(a, Int)]
        zipWithIndex list = zip list [0..]

        digits :: Int -> [Int]
        digits n = map (\ch -> read [ch] :: Int) (show n)

        -- one digit multiplication, supports x in [0; 9], y can be whatever
        odm :: Int -> Int -> Int
        odm x y = case x of
            0 -> 0
            1 -> y
            2 -> y + y
            3 -> y + y + y
            4 -> odm 2 (odm 2 y)
            5 -> odm 2 y + odm 3 y
            6 -> odm 3 (odm 2 y)
            7 -> odm 3 y + odm 4 y
            8 -> odm 4 (odm 2 y)
            9 -> odm 3 (odm 3 y)
            _ -> error "unsupported x in odm"

        shiftByTen :: Int -> Int -> Int
        shiftByTen num 0 = num
        shiftByTen num i | i > 0 = shiftByTen (odm 5 (odm 2 num)) (i - 1)
        shiftByTen _ _  = error "division too hard :("

        flatMap :: (a -> [b]) -> [a] -> [b]
        flatMap _ [] = []
        flatMap f (x : xs) = f x ++ flatMap f xs

        shouldBeNegative = case (a, b) of
            (x, y) | x < 0 && y < 0 -> False
            (x, y) | x > 0 && y > 0 -> False
            _                       -> True

        absResult = sum (flatMap (\(digitA, posA) -> map (\(digitB, posB) -> shiftByTen (odm digitA digitB) (posA + posB)) (zipWithIndex (reverse (digits (abs b))))) (zipWithIndex (reverse (digits (abs a)))))


data Nat = Zero | Succ Nat deriving Read

pincr :: Nat -> Nat
pincr = Succ

pdecr :: Nat -> Nat
pdecr (Succ n) = n
pdecr Zero = error "pdecr zero"

padd :: Nat -> Nat -> Nat
padd a Zero = a
padd a (Succ b) = padd (pincr a) b

psub :: Nat -> Nat -> Nat
psub a Zero = a
psub a (Succ b) = psub (pdecr a) b

pmul :: Nat -> Nat -> Nat
pmul _ Zero = Zero
pmul a (Succ b) = padd a (pmul a b)

pquot :: Nat -> Nat -> Nat
pquot _ 0 = error "divide by zero"
pquot a b | b > a = Zero
pquot a b = Succ (pquot (a - b) b)

prem :: Nat -> Nat -> Nat
prem _ 0 = error "divide by zero"
prem a b | b > a = a
prem a b = prem (a - b) b

psignum :: Nat -> Nat
psignum Zero = Zero
psignum (Succ _) = Succ Zero

pfromInteger :: Integer -> Nat
pfromInteger a | a < 0 = error "convert negative number to Nat"
pfromInteger 0 = Zero
pfromInteger a = Succ (pfromInteger (a - 1))

ptoInteger :: Nat -> Integer
ptoInteger a = read (show a)

pcompare :: Nat -> Nat -> Ordering
pcompare Zero Zero = EQ
pcompare Zero (Succ _) = LT
pcompare (Succ _) Zero = GT
pcompare (Succ a) (Succ b) = pcompare a b

pshow :: Nat -> String
pshow a =
    show (inner a)
    where
        inner :: Nat -> Integer
        inner Zero = 0
        inner (Succ z) = 1 + inner z

instance Num Nat where
    (+) = padd
    (*) = pmul
    (-) = psub
    abs = id
    signum = psignum
    fromInteger = pfromInteger

instance Eq Nat where
    (==) a b = pcompare a b == EQ

instance Ord Nat where
    compare = pcompare

instance Show Nat where
    show = pshow

instance Real Nat where
    toRational a = toRational (ptoInteger a)

instance Enum Nat where
    toEnum i = pfromInteger (toEnum i)
    fromEnum n = fromEnum (ptoInteger n)

instance Integral Nat where
    toInteger = ptoInteger
    quotRem a b = (pquot a b, prem a b)

instance PrintfArg Nat where
    formatArg n = formatArg (toInteger n)