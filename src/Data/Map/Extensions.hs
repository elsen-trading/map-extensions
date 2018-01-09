{-|
  Module      : Data.Map.Extensions
  Description : Extensions to Data.Map
  Copyright   : (c) Elsen, Inc., 2016
  License     : BSD3
  Maintainer  : cooper.charles.m@gmail.com
  Stability   : experimental
  Portability : portable

  This module is a drop-in replacement for `Data.Map`. It is intended to be imported as @import qualified Data.Map.Extensions as Map@.
-}
module Data.Map.Extensions (
  module Data.Map,

  dropLeft,
  dropRight,
  takeLeft,
  takeRight,
  slice,
  slicei,

  filterM,
  transpose,

  scanl1,
  scanr1,

  groupBy,
  groupKeysBy,
  groupElemsBy,

  fromList2,
  fromLists,

  lookup2,
  lookup3,
  lookup4,

  Lookup,
  Lookup2,
  Lookup3,
  Lookup4,
  ) where
import           Prelude hiding (take, drop, scanl1, scanr1, lookup)
import qualified Prelude
import           Data.Map
import qualified Data.Map as Map
import           Data.Set (Set(..))
import qualified Data.List as List

import           Control.Monad ((>=>))

-- | Synonym for Map
type Lookup  ix1 tgt = Map.Map ix1 tgt
-- | A doubly nested Map
type Lookup2 ix1 ix2 tgt = Map.Map ix1 (Map.Map ix2 tgt)
-- | A triply nested Map
type Lookup3 ix1 ix2 ix3 tgt = Map.Map ix1 (Lookup2 ix2 ix3 tgt)
-- | A quadruply nested Map
type Lookup4 ix1 ix2 ix3 ix4 tgt = Lookup2 ix1 ix2 (Lookup2 ix3 ix4 tgt)

ex1 :: (Ord a, Ord b, Ord c) => Lookup3 a b c d -> Lookup3 c b a d
ex1 = fmap transpose . transpose . fmap transpose

ex2 :: (Ord a, Ord b) => Lookup2 a b c -> Lookup2 a b d -> Lookup2 a b (c,d)
ex2 = Map.intersectionWith (Map.intersectionWith (,))

-- | Exclusive slice
--
-- O(log n)
sliceEx :: Ord k => Map.Map k v -> k -> k -> Map.Map k v
sliceEx m l r = let
  (lo, _)  = Map.split r m  -- O(log n)
  (_, mid) = Map.split l lo -- O(log n)
  in mid

-- | Inclusive key-based slice.
-- Returns a map whose keys are all between the
-- lower and upper bounds (inclusive).
--
-- O(log n)
slice :: Ord k => Map.Map k v -> k -> k -> Map.Map k v
slice m l r = let
  (lo, top, _)  = Map.splitLookup r m  -- O(log n)
  (_, bot, mid) = Map.splitLookup l lo -- O(log n)
  ret = case top of
    Nothing -> mid                     -- O(0)
    Just v  -> Map.insert r v mid      -- O(log n)
  ret' = case bot of
    Nothing -> ret                     -- O(0)
    Just v  -> Map.insert l v ret      -- O(log n)
  in if l > r
    then Map.empty
    else ret'

-- | Inclusive index-based slice.
-- Run an inclusive slice given left and right indices.
-- if the left or right index is out of bounds,
-- the left index of @0@ or right index of (@Map.size m - 1@)
-- will be used respectively.
--
-- O(log n)
slicei :: Ord k => Map.Map k v -> Int -> Int -> Map.Map k v
slicei m l sz = let
  r   = l + sz - 1 -- 0 1 -> [0,1)
  len = Map.size m                     -- O(1)
  lv  = fst $ elemAt (max 0       l) m -- O(log n)
  rv  = fst $ elemAt (min (len-1) r) m -- O(log n)
  in case Map.null m of
    True  -> empty
    False -> slice m lv rv                     -- O(log n)

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
-- | Drops `n` elements from the (left hand side of the) `Map`.
dropLeft :: Int -> Map k v -> Map k v
dropLeft n m | n <= 0      = m
             | Map.null m  = m
             | otherwise   = dropLeft (n-1) (deleteMin m)

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
-- | Drops `n` elements from the (right hand side of the) `Map`.
dropRight :: Int -> Map k v -> Map k v
dropRight n m | n <= 0     = m
              | Map.null m = m
              | otherwise  = dropRight (n-1) (deleteMax m)

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
-- | Takes `n` elements from the (left hand side of the) `Map`.
takeLeft :: Int -> Map k v -> Map k v
takeLeft n m = dropRight (Map.size m - n) m

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
-- | Takes `n` elements from the (right hand side of the) `Map`.
takeRight :: Int -> Map k v -> Map k v
takeRight n m = dropLeft (Map.size m - n) m

-- | This generalizes `Map.filter` to a monadic predicate.
filterM :: (Ord k, Monad m) => (v -> m Bool) -> Map k v -> m (Map k v)
filterM pred m = do
  checks <- mapM pred m
  return $ intersection m (Map.filter id checks)

-- | Transpose the first two indexes of a nested `Map`.
transpose :: (Ord a, Ord b) => Lookup2 a b v -> Lookup2 b a v
transpose table = let
  foo = toList . fmap toList $ table
  bar = concatMap (\(a,bvs{-[(b,v)]-}) -> zip (repeat a) bvs) foo
  baz = (\(a,(b,v)) -> (b,a,v)) <$> bar
  in fromList2 baz

-- | Run a grouping function over the keys of a `Map`.
--
-- O(n * log(n))
groupKeysBy :: (Ord a, Ord b) => (a -> b) -> Lookup a v -> Lookup2 b a v
groupKeysBy f = fmap fromList . groupBy (f . fst) . toList

-- | Run a grouping function over the values of a `Map`.
-- 
-- O(n * log(n))
groupElemsBy :: (Ord a, Ord b) => (v -> b) -> Lookup a v -> Lookup2 b a v
groupElemsBy f = fmap fromList . groupBy (f . snd) . toList

-- | Run a grouping function over a `Map`.
-- The supplied function will map each element of the list to a group.
-- The resulting `Map` will map the groups produced by the supplied function
-- to the lists of elements which produced that group.
--
-- Perhaps this is better illustrated by example:
--
-- >>> let even s = s `mod` 2 == 0
-- >>> groupBy even [1,2,3,4]
-- fromList [(False,[3,1]),(True,[4,2])]
--
-- O(n * log(n))
groupBy :: Ord b => (a -> b) -> [a] -> Map b [a]
groupBy f = fromListWith (++) . fmap (\a -> (f a, pure a))

-- | Create a Map from a list of keys and a list of values.
--
-- prop> fromLists ks vs = fromList (zip ks vs)
fromLists :: Ord k => [k] -> [v] -> Map.Map k v
fromLists ks vs = fromList $ zip ks vs

-- | Perform a left scan on the values of a `Map`.
--
-- prop> Map.elems (Map.scanl1 f xs) = List.scanl1 f (Map.elems xs)
scanl1 :: (Ord k) => (a -> a -> a) -> Lookup k a -> Lookup k a
scanl1 f m = let
  (ks, vs) = unzip $ toList m
  in fromList $ zip ks $ Prelude.scanl1 f vs

-- | Perform a right scan on the values of a `Map`.
--
-- prop> Map.elems (Map.scanr1 f xs) = List.scanr1 f (Map.elems xs)
scanr1 :: (Ord k) => (a -> a -> a) -> Lookup k a -> Lookup k a
scanr1 f m = let
  (ks, vs) = unzip $ toList m
  in fromList $ zip ks $ Prelude.scanr1 f vs

-- | Generate a Lookup2 from a list of triples.
fromList2 :: (Ord a, Ord b) => [(a,b,v)] -> Lookup2 a b v
fromList2 xs = let
  ys = (\(a,b,v) -> (a,[(b,v)])) <$> xs
  in fmap fromList $ fromListWith (++) ys

-- | Lookup a value two levels deep in a Lookup2
lookup2 :: (Ord a, Ord b) => a -> b -> Lookup2 a b v -> Maybe v
lookup2 k1 k2 = lookup k1 >=> lookup k2

-- | Lookup a value three levels deep in a Lookup3
lookup3 :: (Ord a, Ord b, Ord c) => a -> b -> c -> Lookup3 a b c v -> Maybe v
lookup3 k1 k2 k3 = lookup k1 >=> lookup k2 >=> lookup k3

-- | Lookup a value four levels deep in a Lookup4
lookup4 :: (Ord a, Ord b, Ord c, Ord d) =>
  a -> b -> c -> d -> Lookup4 a b c d v -> Maybe v
lookup4 k1 k2 k3 k4 = lookup k1 >=> lookup k2 >=> lookup k3 >=> lookup k4

