module Common.Util ( module Common.Util
                   , first
                   , second
                   ) where
import Data.List
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(..), Min(..))
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.Bifunctor (first, second)
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Comonad
import Control.Comonad.Store
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Map (Map)
import Data.Map qualified as M
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Set (Set)
import Data.Set qualified as S

-- | Loeb is just fancy memoization.
-- Each function in @f@ knows how to get a result,
-- provided it has the full structure
loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap

-- | Fancy function that does fancy things.
moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = let go = f ($ go) x in go

-- | Composes all functions in a list from left to right.
composeAll :: Foldable f => f (a -> a) -> a -> a
composeAll = appEndo . getDual . foldMap (Dual . Endo)

-- | Checks if a given value is still in the list after running a filter.
elemIf :: (Foldable t, Eq a) => (a -> Bool) -> a -> t a -> Bool
elemIf p = elemOf (folded . filtered p)

-- | Counts the number of elements in a foldable that satisfy a given predicate.
countIf :: Foldable t => (a -> Bool) -> t a -> Int
countIf p = lengthOf (folded . filtered p)

-- | Computes the sum of all the elements that satisfy a given predicate.
sumIf :: (Foldable t, Num a) => (a -> Bool) -> t a -> a
sumIf = foldMapIf (coerced :: Iso' a (Sum a))
-- | Computes the product of all the elements that satisfy a given predicate.
productIf :: (Foldable t, Num a) => (a -> Bool) -> t a -> a
productIf = foldMapIf (coerced :: Iso' a (Product a))
-- | Computes the maximum of all the elements that satisfy a given predicate.
maximumIf :: (Foldable t, Ord a, Bounded a) => (a -> Bool) -> t a -> a
maximumIf = foldMapIf $ iso Max getMax
-- | Computes the minimum of all the elements that satisfy a given predicate.
minimumIf :: (Foldable t, Ord a, Bounded a) => (a -> Bool) -> t a -> a
minimumIf = foldMapIf $ iso Min getMin
-- | Mappends all elements that satisfy a predicate
foldIf :: (Foldable t, Monoid m) => (m -> Bool) -> t m -> m
foldIf = foldMapIf id

-- | Mappends all elements that satisfy a predicate
-- Uses an iso to create the monoid
foldMapIf :: (Foldable t, Monoid m)
           => Iso' a m
           -> (a -> Bool)
           -> t a
           -> a
foldMapIf _monoided p xs =
  _monoided # foldOf (folded . filtered p . _monoided) xs

-- | Gets the indices of all elements satisfying the predicate
indicesWhere :: Foldable f => (a -> Bool) -> f a -> [Int]
indicesWhere p xs = xs ^.. folded . filtered p . asIndex

-- | Converts a given sequence of digits from base @n@ to base @10@
fromBase :: (Num n, Foldable f) => n -> f n -> n
fromBase base = foldl' (\acc b -> base * acc + b) 0

-- | Converts a given number from base @10@ to base @n@
toBase :: Integral n => n -> n -> [n]
toBase base = reverse . unfoldr go
  where
    go n
      | n == 0 = Nothing -- Stop when previous division returned 0
      | otherwise = Just (mod, div) -- Keep the mod, use the div
      where
        (div, mod) = n `divMod` base

-- | Implementation of 'Data.List.nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub xs = foldr f (const []) xs S.empty
  where
    f x recur seen =
      case recur <$> S.alterF (, True) x seen of
        (True,  ys) -> ys
        (False, ys) -> x : ys

-- | Returns all the adjacent pairs of elements
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- | Returns non-overlapping chunks of length n
-- The last chunk might be incomplete
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (ys,rest) -> ys : chunks n rest

-- | Does a given Map match what's in a Store?
matchMap :: (Num k, Eq a) => Map k a -> Store k (Maybe a) -> Bool
matchMap m = getAll . storeMapNeighborhood ((\x y -> All (Just x == y)) <$> m)

-- | For every key in the map, look it up in the store
-- then run the given function, mappending the results
storeMapNeighborhood ::
  (Num k, Monoid b) =>
  Map k (Maybe a -> b) ->
  Store k (Maybe a) ->
  b
storeMapNeighborhood m s = M.foldMapWithKey (\p f -> f $ peeks (+ p) s) m

-- | Turn a map into a store
mapToStore :: (Ord k, Num k) => Map k a -> Store k (Maybe a)
mapToStore m = store (m M.!?) 0

-- | Turn a store into a map
mapFromStore :: Num k => Set k -> Store k a -> Map k a
mapFromStore s = experiment $ \x -> M.fromSet (+ x) s

-- | Checks if all elements are equal.
same :: (Foldable t, Eq a) => t a -> Bool
same xs = all (head (toList xs) ==) xs

-- | Checks if all elements are different.
uniq :: (Foldable t, Ord a) => t a -> Bool
uniq xs = length xs == S.size (S.fromList $ toList xs)

-- | Finds all the configurations where each @k@ is given a different @a@
pickUnique :: forall k a. (Ord k, Ord a) => Map k (Set a) -> [Map k a]
pickUnique m = flip evalStateT S.empty $ do
    fmap M.fromList . for opts . traverse $ \poss -> do
      seen <- get
      pick <- lift $ S.toList (poss `S.difference` seen)
      pick <$ modify (S.insert pick)
  where
    -- Pick from the smallest sets first
    opts :: [(k, Set a)]
    opts = sortOn (S.size . snd) $ M.toList m

-- | Finds the first element where the function returns a @Just@ value
firstJust :: Foldable t
          => (a -> Maybe b)
          -> t a
          -> Maybe b
firstJust p = asum . map p . toList

-- | Finds the first fixed point of a given function.
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f !x
  | x == x'   = x
  | otherwise = fixPoint f x'
  where x' = f x

-- | Finds the number of iterations until the
-- first fixed point of a given function.
fixPointLength :: Eq a => (a -> a) -> a -> Int
fixPointLength = go 1
  where
    go n f !x
      | x == x'   = n
      | otherwise = go (n+1) f x'
      where x' = f x

-- | Counts the number of times each element appears in a given foldable.
freqs :: (Foldable t, Ord a) => t a -> Map a Int
freqs = foldl' (\m val -> M.insertWith (+) val 1 m) M.empty

-- | Counts the number of times each Int appears in a given foldable.
intFreqs :: Foldable t => t Int-> IntMap Int
intFreqs = foldl' (\m val -> IM.insertWith (+) val 1 m) IM.empty

-- | Build a reverse frequency map
revFreqs :: (Foldable f, Ord a) => f a -> IntMap (NESet a)
revFreqs = IM.fromListWith (<>)
         . map (swap . first NES.singleton)
         . M.toList
         . freqs

-- | Build a list of /descending/ frequencies.  Ties are sorted.
freqList :: (Foldable f, Ord a) => f a -> [(Int, a)]
freqList = concatMap (traverse toList) . IM.toDescList . revFreqs

-- | Fsnds all the ways to select one element from a list
-- Returns the selected value and list of the other values
-- NOTE: The list of other values isn't in the same order as the input list
select :: [a] -> [(a,[a])]
select = go []
  where
    go _  [] = []
    go xs (y:ys) = (y, xs ++ ys) : go (y:xs) ys

-- | Gets all single item perterbations of the elements
perturbations
    :: Each s t a a
    => (a -> [a]) -- ^ Perturnbation function
    -> s          -- ^ Structure to perturb
    -> [t]        -- ^ Perturbations
perturbations = perturbationsBy each

-- | Gets all single item perterbations of the elements
-- Uses a given lens to get elements to change
perturbationsBy
    :: Conjoined p
    => Over p (Bazaar p a a) s t a a -- ^ Lens
    -> (a -> [a])                    -- ^ Perturnbation function
    -> s                             -- ^ Structure to perturb
    -> [t]                           -- ^ Perturbations
perturbationsBy p f = experiment f <=< holesOf p

-- | Iterates until a result of @Nothing@ is produced.
-- This is an inifitie loop if a @Just a@ result is never produced.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f z = z : unfoldr (fmap dup . f) z

-- | Like @scanl@, but for Traverable structures
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z

-- | Like @scanr@, but for Traverable structures
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z

dup :: a -> (a, a)
dup x = (x, x)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | Like @maximum@, but it checks for an empty structure
maximumMaybe :: (Ord a, Foldable t) => t a -> Maybe a
maximumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! maximum xs

-- | Like @minimum@, but it checks for an empty structure
minimumMaybe :: (Ord a, Foldable t) => t a -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs

-- | Returns the last value for which a @Just@ result was given.
-- This is an infinite loop if no @Just@ is ever produced.
lastJust :: (a -> Maybe a) -> a -> a
lastJust f = go
  where
    go !x = case f x of
              Nothing -> x
              Just !y -> go y

-- | Finds the first element that appears at least twice.
firstDup :: Ord a => [a] -> Maybe a
firstDup = firstDupBy id

-- | Finds the first repeated element in the list
-- Uses a projection function to get only relevent info for comparison
firstDupBy :: Ord b => (a -> b) -> [a] -> Maybe a
firstDupBy f = go S.empty
  where
    go seen (x:xs)
      | f x `S.member` seen = Just x
      | otherwise           = go (f x `S.insert` seen) xs
    go _ [] = Nothing

-- | Find the first and second index that a
-- duplicate element is found
findCycle :: Ord a => [a] -> (Int, Int)
findCycle = go M.empty 0
  where
    go _ _ [] = error "No cycle"
    go seen i (x:xs) =
      case seen M.!? x of
        Nothing -> go (M.insert x i seen) (i + 1) xs
        Just j  -> (j, i)

-- | Strict Iterate
strictIterate :: (a -> a) -> a -> [a]
strictIterate f = go
  where
    go !x = x : go (f x)

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[]     !!! _ = error "Out of range"
(x:_)  !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Apply a function @n@ times strictly.
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x

-- | Gives head, or a default value
headOr :: a -> [a] -> a
headOr def []    = def
headOr _   (x:_) = x
