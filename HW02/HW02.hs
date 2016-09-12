{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
    | x == y = 1 + exactMatches xs ys
    | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = countHelper colors code
    where countHelper :: [Peg] -> [Peg] -> [Int]
          countHelper [] _ = []
          countHelper (x:xs) l = (length $ filter (==x) l):countHelper xs l


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xl yl = let countX = countColors xl
                    countY = countColors yl
                in go 0 countX countY
                    where
                      go :: Int -> [Int] -> [Int] -> Int
                      go cnt [] [] = cnt
                      go cnt [] _ = cnt
                      go cnt _ [] = cnt
                      go cnt (x:xs) (y:ys) = go (cnt + (min x y)) xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys =  Move ys exact (matched - exact)
    where
        exact = exactMatches xs ys
        matched =  matches xs ys

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code =
    let Move guess _ _ = move in
    getMove code guess == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\x -> [x]) colors
allCodes n = concatMap (\x -> map(\y -> x ++ [y]) colors) (allCodes (n-1))
-- allCodes num
--  | n <= 0 = []
--  | n == 1 = [[x]: x <- colors]
--  | otherwise = [(x:y) | x <- colors , y <- allCodes (n-1)]


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code =
    let allcodes = allCodes (length code) in
    go allcodes
    where
        go :: [Code] -> [Move]
        go [] = []
        go (x:[]) = [getMove code x]
        go (x:xs) = (getMove code x):(go (filterCodes (getMove code x) xs))



-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
