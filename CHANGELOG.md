0.2.0.0
Remove functions which are superseded by upstream

`containers` has added take, drop, restrictKeys and withoutKeys which are
equivalent to take, drop, keepKeys and dropKeys from the map-exts API,
respectively. takeLeft/Right and dropLeft/Right have been exposed.

containers:drop/take correspond to dropLeft and takeLeft respectively.

map-exts is now compatible with lts-10.3

0.1.1.0
Add version bound for upstream containers

0.1.0.1
Improve documentation, examples
