# `miniBill/elm-fast-dict` [![Build Status](https://github.com/miniBill/elm-fast-dict/workflows/CI/badge.svg)](https://github.com/miniBill/elm-fast-dict/actions?query=branch%3Amain)

This is a drop-in (except point 1 below) replacement for `Dict` from `elm/core`. The suggested way to use it is by replacing `import Dict` with `import FastDict as Dict` in your project.

The main differences are:

1. This is not a built-in type, so you can't use `==` with it. If you do you may get false negatives (dictionaries which are equal but that `==` considers different) - use `FastDict.equals` to check for equality;
2. `size` is `O(1)` instead of `O(n)` (i.e.: it runs in constant time instead of scanning the whole dictionary);
3. `union` automatically merges the smaller dictionary into the bigger (this doesn't change the fact that in case of conflict the values from the first dictionary are picked), making it faster;
4. `intersect` is MUCH faster for dictionaries with little intersection - in particular if one of the two dictionaries is much smaller than the other;
5. `equals` can be much faster: it's `O(1)` if the size is different, and `O(index of first different value)` otherwise;
6. `getMinKey/getMin/popMin/getMaxKey/getMax/popMax` functions are available, with cost `O(log n)`.

# When to use this package

- You use `intersect`, `union` or `size` a lot;
- you have big dictionaries;
- you need a fast `getMin/getMax` function.

# When not to use this package

- You need to interact with code that expects `elm/core` dictionaries a lot;
- you have tiny dictionaries;
- you have a lot of existing code that would need to be checked for uses of `==`.

# Examples

```elm
import FastDict as Dict exposing (Dict)

type alias Priority =
    Int

type alias Job =
    String

queue : Dict Priority Job
queue =
    Dict.fromList
        [ (3, "Shave the yak")
        , (5, "Reticulate splines")
        , (1, "Feed the gremlins")
        ]

{-| Returns the most important item

    mostImportant queue
    --> Just (1, "Feed the gremlins")

-}

mostImportant : Dict Priority Job -> Maybe (Priority, Job)
mostImportant =
    Dict.getMin
```
