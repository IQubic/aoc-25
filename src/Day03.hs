module Day03 where

import Common.Runner
import Common.Util
import Common.Parser
import Data.List (tails)
import Data.MemoTrie (memo2)

part1 :: String -> Int
part1 = getAns 2 . pInput

part2 :: String -> Int
part2 = getAns 12 . pInput

-- | Given a number of digits to pick
-- and a number of rows to analyse,
-- find the maximum subsequence
getAns :: Int -> [String] -> Int
getAns n = sum . map (read . best n)
  where
    -- Find the best way to select the remaning N digits
    best = memo2 go
    go :: Int -> String -> String
    go 0 s = "0"
    go 1 s = [maximum s]
    -- Try each possible head with all possible tails
    go n s =
      maximum [ c : best (n-1) rest
              | c:rest <- tails s
              , length rest >= n-1]

pInput :: String -> [String]
pInput = pLines (some $ anySingleBut '\n')

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 3
