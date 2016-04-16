# map-extensions

This module contains some extensions to Data.Map. Some of them are convenience functions.

It also contains functions to support a split-apply-combine workflow, by representing labeled, multi-dimensional data as multiply nested `Map`s. For instance, a two dimensional matrix with one axis indexed by 'Name's and the other axis labeled by 'Job's, we would represent such a structure with a Lookup2 Name Job Double. Such a structure is not terribly efficient (it takes O(n log(n)) space and O(log(n)) time for insert/update/delete operations with high constant factor owing to all the pointer manipulation), but it is expressive.

Here is the rough correspondence between split-apply-combine and the verbs used in this package:

`split`   : `groupBy`

`apply`   : `fmap`

`combine` : `foldr/foldMap`

`reshape` : `transpose`

For more information, see [the examples](https://github.com/elsen-trading/map-extensions/tree/master/examples).
