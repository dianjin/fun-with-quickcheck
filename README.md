# fun-with-quickcheck

### Purpose
Simple demo of Haskell QuickCheck. Problem: implement and test a function that simplifies a list of time intervals.

Example inputs and outputs:
* [(1, 5), (5, 8)] -> [(1, 8)]
* [(1, 2), (3, 5)] -> [(1, 2), (3, 5)]
* [(1, 5), (2, 4)] -> [(1, 5)]

### Running tests
```
stack test
```

### Blog post
[Fun with Haskell QuickCheck](https://dianjin.github.io/fun-with-haskell-quickcheck/)
