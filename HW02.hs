{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List (group, sort)

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
exactMatches actual guess = match $ zip actual guess
    where match [] = 0
          match ((x, y) : xs)
              | x == y = 1 + match xs
              | otherwise = match xs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = replicate 6 0
countColors (Red : xs) = let [r, g, b, y, o, p] = countColors xs in [r + 1, g, b, y, o, p]
countColors (Green : xs) = let [r, g, b, y, o, p] = countColors xs in [r, g + 1, b, y, o, p]
countColors (Blue : xs) = let [r, g, b, y, o, p] = countColors xs in [r, g, b + 1, y, o, p]
countColors (Yellow : xs) = let [r, g, b, y, o, p] = countColors xs in [r, g, b, y + 1, o, p]
countColors (Orange : xs) = let [r, g, b, y, o, p] = countColors xs in [r, g, b, y, o + 1, p]
countColors (Purple : xs) = let [r, g, b, y, o, p] = countColors xs in [r, g, b, y, o, p + 1]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ zipWith min (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = let exact = exactMatches actual guess in Move guess exact (matches actual guess - exact)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move a _ _) b = m == getMove b a

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
allCodes n = concatMap (\c -> map (: c) clrs) $ allCodes (n - 1)
    where clrs = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve actual = step (replicate lencode Red) (allCodes lencode)
    where lencode = length actual
          step guess codes = let m = getMove actual guess
                                 cs = filterCodes m codes
                             in if guess == actual then [m] else m : step (head cs) (tail cs)


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess actual = step initial (allCodes lencode) (filter (/= initial) $ allCodes lencode)
    where lencode = length actual 
          initial = replicate (lencode `quot` 2) Red ++ replicate (lencode - lencode `quot` 2) Green
          step guess codes unused = let m = getMove actual guess
                                        s = filterCodes m codes
                                        (_, _, highest) = scores s unused
                                    in if guess == actual then [m] else m : step highest s (filter (/=highest) unused)
          scores s cs = maximum $ map (\c -> (score s c, c `elem` s, c)) cs
          score s code =  length s - maximum (hitcounts code s)
          hits code = map (\si -> let (Move _ ex nex) = getMove si code in (ex, nex))
          hitcounts code s = map length (group . sort $ hits code s)
