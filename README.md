# `miniBill/elm-fast-dict` [![Build Status](https://github.com/miniBill/elm-fast-dict/workflows/CI/badge.svg)](https://github.com/miniBill/elm-fast-dict/actions?query=branch%3Amain)

This is a replacement package for `Dict` from `elm/core`. The API is identical, except needing to use `equals` instead of `==` for comparing two dictionaries.

The suggested way to use this package is by replacing `import Dict` with `import FastDict as Dict` in your project.

The main differences between `Dict` and `FastDict` are:

1. This is not a built-in type, so you can't use `==` with it. If you do you may get false negatives (dictionaries which are equal but that `==` considers different) - use `FastDict.equals` to check for equality;
2. `size` is `O(1)` instead of `O(n)` (i.e.: it runs in constant time instead of scanning the whole dictionary);
3. `union` automatically merges the smaller dictionary into the bigger (without changing the result), making it faster;
4. `intersect` is `O(m + n)` instead of `O(m log n)` and in practice is MUCH faster for small intersections (usually ranging from 2x faster to 100x faster);
5. `equals` is sometimes faster: it's `O(1)` if the size is different, and `O(index of first different value)` otherwise instead of `O(m + n)`;
6. `getMinKey/getMin/popMin/getMaxKey/getMax/popMax` functions are available, with cost `O(log n)`,
7. `stoppableFoldl/stoppableFoldr/restructure` functions are available.

# When to use this package

- You use `intersect`, `union` or `size` a lot;
- you have big dictionaries;
- you need a fast `getMin/getMax` function;
- you need `stoppableFold`s or `restructure`.

# When not to use this package

- You need to interact with code that expects `elm/core` dictionaries a lot;
- you have tiny dictionaries;
- you have a lot of existing code that would need to be checked for uses of `==`;
- you need to use `Html.Lazy`.

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
