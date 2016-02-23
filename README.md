# map-extensions

This module contains some extensions to Data.Map. Some of them are convenience functions.

It also contains functions to support a split-apply-combine workflow, by representing labeled, multi-dimensional data as multiply nested `Map`s. For instance, a two dimensional matrix with one axis indexed by 'Name's and the other axis labeled by 'Job's, we would represent such a structure with a Lookup2 Name Job Double. Such a structure is not terribly efficient (it takes O(n log(n)) space and O(log(n)) time for insert/update/delete operations with high constant factor owing to all the pointer manipulation), but it is expressive.

`split`   : `groupMapBy`
`apply`   : `fmap`
`combine` : `foldr/foldMap`
`reshape` : `transpose`

Say we had a dataFrame :: (Lookup2 Name Job Age). We could group by Job by running transpose on it. If we wanted to find the average Age in a job, we could define

```haskell
mean :: Map k v -> Int
mean xs = foldr (+) 0 xs / Map.size xs

type Name = String
type Job  = String
type Age  = Double
averageAgeInJob :: Lookup2 Name Job Age -> Map Job Age
averageAgeInJob dataFrame = fmap mean $ transpose dataFrame
```

This illustrates applying an aggregation over a particular index by reshaping with `transpose`, defining a fold, and then focusing it inside the structure using `fmap`.

