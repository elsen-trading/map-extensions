-- extensions to Data.Map.Lazy
module Data.Map.Extensions (
  module Data.Map,

  drop,
  take,
  slice,
  slicei,

  keepKeys,
  dropKeys,


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

-- these are a bit clunky to read and write. but great for the typechecker
type Lookup  ix1 tgt = Map.Map ix1 tgt
type Lookup2 ix1 ix2 tgt = Map.Map ix1 (Map.Map ix2 tgt)
type Lookup3 ix1 ix2 ix3 tgt = Map.Map ix1 (Lookup2 ix2 ix3 tgt)
type Lookup4 ix1 ix2 ix3 ix4 tgt = Lookup2 ix1 ix2 (Lookup2 ix3 ix4 tgt)

ex1 :: (Ord a, Ord b, Ord c) => Lookup3 a b c d -> Lookup3 c b a d
ex1 = fmap transpose . transpose . fmap transpose

ex2 :: (Ord a, Ord b) => Lookup2 a b c -> Lookup2 a b d -> Lookup2 a b (c,d)
ex2 = Map.intersectionWith (Map.intersectionWith (,))

-- exclusive slice
-- O(log n)
sliceEx :: Ord k => Map.Map k v -> k -> k -> Map.Map k v
sliceEx m l r = let
  (lo, _)  = Map.split r m  -- O(log n)
  (_, mid) = Map.split l lo -- O(log n)
  in mid

-- inclusive slice
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
  in ret'

-- O(log n)
-- slices on left and right indices.
-- if the left or right index is out of bounds,
-- the left index of 0 or right index of (Map.size m - 1)
-- will be used respectively
slicei :: Ord k => Map.Map k v -> Int -> Int -> Map.Map k v
slicei m l sz = let
  r   = l + sz - 1 -- 0 1 -> [0,1)
  len = Map.size m                    -- O(1)
  lv  = fst $ elemAt (max 0       l) m -- O(log n)
  rv  = fst $ elemAt (min (len-1) r) m -- O(log n)
  in slice m lv rv                    -- O(log n)

data OneOrBoth a b = First a | Second b | Both a b
outerJoinWith :: Ord k => (OneOrBoth a b -> c) -> Map.Map k a -> Map.Map k b -> Map.Map k c
outerJoinWith f = Map.mergeWithKey (\_ a b -> Just $ f (Both a b)) (f . First <$>) (f . Second <$>)

dropLeft :: Int -> Map k v -> Map k v
dropLeft n m | n <= 0      = m
             | Map.null m  = m
             | otherwise   = dropLeft (n-1) (deleteMin m)

dropRight :: Int -> Map k v -> Map k v
dropRight n m | n <= 0     = m
              | Map.null m = m
              | otherwise  = dropRight (n-1) (deleteMax m)

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
drop :: Int -> Map k v -> Map k v
drop = dropLeft

-- O(n * log n). this could be O(log n) if we used Map.split cleverly.
take :: Int -> Map k v -> Map k v
take n m = dropRight (Map.size m - n) m

filterM :: (Ord k, Monad m) => (v -> m Bool) -> Map k v -> m (Map k v)
filterM pred m = do
  checks <- mapM pred m
  return $ intersection m (Map.filter id checks)

-- this is a scalable, typesafe way to munge data.
-- e.g. if we had Lookup3 Inst.Equity Day Code Double
-- and we wanted Lookup3 Code Day Inst.Equity Double,
-- we would simply write
-- fmap transpose {-Code Inst.Equity Day -> Code Day Inst.Equity-}
-- . transpose {-Inst.Equity Code Day -> Code Inst.Equity Day-}
-- . fmap transpose {-Inst.Equity Day Code -> Inst.Equity Code Day-}
-- General rule is, if it typechecks, it's definitely correct.
-- As another example, if we wanted to inner join like so:
-- Lookup2 Inst.Equity Day Double -> Lookup2 Inst.Equity Day Int
-- -> Lookup2 Inst.Equity Day (Double, Int)
-- we could write
-- intersectionWith (intersectionWith (,))
--
-- ^NEEDS TEST^
transpose :: (Ord a, Ord b) => Lookup2 a b v -> Lookup2 b a v
transpose table = let
  foo = toList . fmap toList $ table
  bar = concatMap (\(a,bvs{-[(b,v)]-}) -> zip (repeat a) bvs) foo
  baz = (\(a,(b,v)) -> (b,a,v)) <$> bar
  in fromList2 baz

-- O(n * log(n))
groupKeysBy :: (Ord a, Ord b) => (a -> b) -> Lookup a v -> Lookup2 b a v
groupKeysBy f = fmap fromList . groupBy (f . fst) . toList

groupElemsBy :: (Ord a, Ord b) => (v -> b) -> Lookup a v -> Lookup2 b a v
groupElemsBy f = fmap fromList . groupBy (f . snd) . toList

-- Could this be generalized to traversable?
groupBy :: Ord b => (a -> b) -> [a] -> Map b [a]
groupBy f = fromListWith (++) . fmap (\a -> (f a, pure a))

keepKeys :: Ord k => Set k -> Map k a -> Map k a
keepKeys keys m = intersection m (fromSet (const ()) keys)

dropKeys :: Ord k => Set k -> Map k a -> Map k a
dropKeys keys m = m \\ (fromSet (const ()) keys)

fromLists :: Ord k => [k] -> [v] -> Map.Map k v
fromLists ks vs = fromList $ zip ks vs

scanl1 :: (Ord k) => (a -> a -> a) -> Lookup k a -> Lookup k a
scanl1 f m = let
  (ks, vs) = unzip $ toList m
  in fromList $ zip ks $ Prelude.scanl1 f vs

scanr1 :: (Ord k) => (a -> a -> a) -> Lookup k a -> Lookup k a
scanr1 f m = let
  (ks, vs) = unzip $ toList m
  in fromList $ zip ks $ Prelude.scanr1 f vs

fromList2 :: (Ord a, Ord b) => [(a,b,v)] -> Lookup2 a b v
fromList2 xs = let
  ys = (\(a,b,v) -> (a,[(b,v)])) <$> xs
  in fmap fromList $ fromListWith (++) ys

lookup2 :: (Ord a, Ord b) => a -> b -> Lookup2 a b v -> Maybe v
lookup2 k1 k2 = lookup k1 >=> lookup k2

lookup3 :: (Ord a, Ord b, Ord c) => a -> b -> c -> Lookup3 a b c v -> Maybe v
lookup3 k1 k2 k3 = lookup k1 >=> lookup k2 >=> lookup k3

lookup4 :: (Ord a, Ord b, Ord c, Ord d) =>
  a -> b -> c -> d -> Lookup4 a b c d v -> Maybe v
lookup4 k1 k2 k3 k4 = lookup k1 >=> lookup k2 >=> lookup k3 >=> lookup k4

