module Day01 where

import Common.Runner
import Common.Parser
import Common.Util (countIf)
import Data.List (mapAccumL)

data Turn = L Int | R Int deriving Show

part1 :: String -> Int
part1 = countIf (== 0) .
        scanl turn 50 .
        pInput
  where
    -- Just turn N steps and mod to stay in range
    turn start (L n) = (start - n) `mod` 100
    turn start (R n) = (start + n) `mod` 100

-- Here we discard the final state just use
-- The number of 0 hits per turn
part2 :: String -> Int
part2 = sum . snd .         
        mapAccumL turn 50 .
        pInput
  where
    -- We use sym to "flip the lock" so that all turns are R turns
    -- This lets us avoid negatives 
    -- divMod does weird things with negative numbers
    turn start (L n) = 
      let (zeros, end) = (sym start + n) `divMod` 100
      in (sym end, zeros)
    turn start (R n) =
      let (zeros, end) = (start + n) `divMod` 100
      in (end, zeros)
    -- "flip the lock"
    sym n = (-n) `mod` 100

pInput :: String -> [Turn]
pInput = pLines $ do
  f <- choice [ char 'L' $> L
              , char 'R' $> R
              ]
  f <$> pNumber

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 1  
