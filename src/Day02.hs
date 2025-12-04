module Day02 where

import Common.Runner
import Common.Parser
import Common.Util (sumIf, chunks, same)

part1 :: String -> Int
part1 = getAns invalid . pInput
  where
    invalid s = uncurry (==) $ splitAt (length s `div` 2) s

part2 :: String -> Int
part2 = getAns invalid . pInput
  where
    invalid s =
      or [same $ chunks n s | n <- [1..length s `div` 2]
                            , (n `mod` length s) == 0]

-- | Given a predicate and some IDs, solve
getAns :: (String -> Bool) -> [Int] -> Int
getAns pred = sumIf (pred . show)

pInput :: String -> [Int]
pInput = concat . pAllInput (pLine $ commaSep pRange)
  where
    pRange :: Parser [Int]
    pRange = do
      lo <- pNumber <* char '-'
      enumFromTo lo <$> pNumber

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
