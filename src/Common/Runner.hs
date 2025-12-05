module Common.Runner ( getInput
                     , submitAnswer
                     , runSolutionOnInput
                     , AoCError
                     ) where
import Advent
import Data.Text qualified as T

myOpts :: AoCOpts
myOpts = defaultAoCOpts myAgent 2025 ""
  where
    myAgent = AoCUserAgent (T.pack "https://github.com/IQubic") (T.pack "sophia.b.caspe@gmail.com")

-- | Gets puzzle input for a given day
getInput :: Integer -- ^ Day
         -> IO (Either AoCError String)
getInput day = let input = runAoC myOpts $ AoCInput (mkDay_ day) in
    (fmap . fmap) T.unpack input

-- | Runs a solution on a given day's input
runSolutionOnInput
  :: Show a
  => Integer       -- ^ Day
  -> (String -> a) -- ^ Solver
  -> IO (Either AoCError a)
runSolutionOnInput day solver = (fmap . fmap) solver (getInput day)

-- | Sumbits an answer to the AoC server
submitAnswer
  :: Show a
  => Integer -- ^ Day
  -> Integer -- ^ Part
  -> a
  -> IO (Either AoCError (T.Text, SubmitRes))
submitAnswer day part ans = runAoC myOpts
  $ AoCSubmit (mkDay_ day) (mkPart_ part) (show ans)
  where
    mkPart_ 1 = Part1
    mkPart_ 2 = Part2
    mkPart_ _ = error "Invalid part"
