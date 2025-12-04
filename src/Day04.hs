module Day04 where

import Common.Runner
import Common.Parser
import Common.Util (countIf, fixPoint)
import Common.Grid
import Data.Set (Set)
import Data.Set qualified as S

part1 :: String -> Int
part1 i = S.size g - S.size next
  where
    g = pInput i
    next = remove g

part2 :: String -> Int
part2 i = S.size g - S.size final
  where
    g = pInput i
    final = fixPoint remove g

-- Keep all the points that can't be removed
remove :: Set Point -> Set Point
remove g = S.filter ((>= 4) . neighborCount) g
  where
    neighborCount =
      countIf (`S.member` g) . fullNeighbors 

pInput :: String -> Set Point
pInput = asciiGridSet (== '@')

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 4
