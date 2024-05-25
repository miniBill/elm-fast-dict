module FastSet exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size, equals
    , union, intersect, diff
    , toList, fromList
    , getMin, getMax
    , popMin, popMax
    , map, foldl, foldr, filter, partition
    , toCoreSet, fromCoreSet
    , stoppableFoldl, stoppableFoldr
    )

{-| A set of unique values. The values can be any comparable type.
This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size, equals


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Min / Max

@docs getMin, getMax

@docs popMin, popMax


# Transform

@docs map, foldl, foldr, filter, partition


# Interoperability

@docs toCoreSet, fromCoreSet


# Advanced functions

@docs stoppableFoldl, stoppableFoldr

-}

import FastDict
import Set



{- Parts of this file (the documentation and API, and much of the implementation) are copied or adapted from `elm/core`, and thus they are Copyright 2014-present Evan Czaplicki -}
-- SETS


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and `(Set String)` is a set of strings.
-}
type Set t
    = Set (FastDict.Dict t Bool)


{-| Create an empty set.
-}
empty : Set t
empty =
    Set FastDict.empty


{-| Determine if a value is in a set.
-}
member : comparable -> Set comparable -> Bool
member key (Set dict) =
    case FastDict.get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of elements in a set.
-}
size : Set t -> Int
size (Set set) =
    FastDict.size set


{-| Determine if two sets are equal. This is needed because the structure could be different depending on insertion order.
-}
equals : Set comparable -> Set comparable -> Bool
equals (Set l) (Set r) =
    FastDict.equals l r


{-| Gets the smallest value.

    [ 1, 2 ]
        |> fromList
        |> getMin
    --> Just 1


    empty
        |> getMin
    --> Nothing

-}
getMin : Set v -> Maybe v
getMin (Set set) =
    FastDict.getMinKey set


{-| Gets the biggest value.

    [ 1, 2 ]
        |> fromList
        |> getMax
    --> Just 2


    empty
        |> getMax
    --> Nothing

-}
getMax : Set v -> Maybe v
getMax (Set set) =
    FastDict.getMaxKey set


{-| Removes the smallest value from the set, and returns it.

    [ 1, 2 ]
        |> fromList
        |> popMin
    --> Just ( 1, fromList [ 2 ] )


    empty
        |> popMin
    --> Nothing

-}
popMin : Set comparable -> Maybe ( comparable, Set comparable )
popMin (Set set) =
    case FastDict.popMin set of
        Nothing ->
            Nothing

        Just ( ( value, _ ), newSet ) ->
            Just ( value, Set newSet )


{-| Removes the biggest value from the set, and returns it.

    [ 1, 2 ]
        |> fromList
        |> popMax
    --> Just ( 2, fromList [ 1 ] )


    empty
        |> popMax
    --> Nothing

-}
popMax : Set comparable -> Maybe ( comparable, Set comparable )
popMax (Set set) =
    case FastDict.popMax set of
        Nothing ->
            Nothing

        Just ( ( value, _ ), newSet ) ->
            Just ( value, Set newSet )


{-| Determine if a set is empty.

    isEmpty empty
    --> True

-}
isEmpty : Set k -> Bool
isEmpty (Set set) =
    FastDict.isEmpty set


{-| Insert a value into a set.
-}
insert : comparable -> Set comparable -> Set comparable
insert value (Set set) =
    Set (FastDict.insert value False set)


{-| Remove a value from a set. If the value is not found,
no changes are made.
-}
remove : comparable -> Set comparable -> Set comparable
remove value (Set set) =
    Set (FastDict.remove value set)


{-| Create a set with one value.
-}
singleton : comparable -> Set comparable
singleton value =
    Set (FastDict.singleton value False)



-- COMBINE


{-| Get the union of two sets. Keep all values.
-}
union : Set comparable -> Set comparable -> Set comparable
union (Set l) (Set r) =
    Set (FastDict.union l r)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set comparable -> Set comparable -> Set comparable
intersect (Set l) (Set r) =
    Set (FastDict.intersect l r)


{-| Get the difference between the first set and the second. Keeps values that do not appear in the second set.
-}
diff : Set comparable -> Set comparable -> Set comparable
diff (Set l) (Set r) =
    Set (FastDict.diff l r)



-- TRANSFORM


{-| Apply a function to all values in a set.
-}
map : (comparable -> comparable2) -> Set comparable -> Set comparable2
map func (Set set) =
    FastDict.foldl (\v _ -> insert (func v)) empty set


{-| Fold over the values in a set from lowest to highest.
-}
foldl : (v -> b -> b) -> b -> Set v -> b
foldl func acc (Set set) =
    FastDict.foldl (\v _ iacc -> func v iacc) acc set


{-| Fold over the values in a set from highest to lowest.
-}
foldr : (v -> b -> b) -> b -> Set v -> b
foldr func acc (Set set) =
    FastDict.foldr (\v _ iacc -> func v iacc) acc set


{-| Keep only the values that pass the given test.
-}
filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter isGood (Set set) =
    Set (FastDict.filter (\v _ -> isGood v) set)


{-| Partition a set according to some test. The first set
contains all values which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable -> Bool) -> Set comparable -> ( Set comparable, Set comparable )
partition isGood (Set set) =
    let
        ( l, r ) =
            FastDict.partition (\v _ -> isGood v) set
    in
    ( Set l, Set r )



-- LISTS


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set k -> List k
toList (Set set) =
    FastDict.keys set


{-| Convert an association list into a set.
-}
fromList : List comparable -> Set comparable
fromList list =
    List.foldl insert empty list



-- INTEROPERABILITY


{-| Convert the set into an equivalent one from elm/core.
-}
toCoreSet : Set comparable -> Set.Set comparable
toCoreSet set =
    foldl Set.insert Set.empty set


{-| Convert the set from an equivalent one from elm/core.
-}
fromCoreSet : Set.Set comparable -> Set comparable
fromCoreSet set =
    Set.foldl insert empty set



-- ADVANCED


{-| A foldl that can stop early instead of traversing the whole set.

    stoppableFoldl
        (\v acc ->
            if v >= 10 then
                Stop acc
            else
                Continue (v + acc)
        )
        0
        (fromList <| List.range 1 10000)
    --> 55

-}
stoppableFoldl : (v -> acc -> FastDict.Step acc) -> acc -> Set v -> acc
stoppableFoldl func acc (Set set) =
    FastDict.stoppableFoldl (\v _ iacc -> func v iacc) acc set


{-| A foldr that can stop early instead of traversing the whole set.

    stoppableFoldr
        (\v acc ->
            if v <= 9990 then
                Stop acc
            else
                Continue (v + acc)
        )
        0
        (fromList <| List.range 1 10000)
    --> 89964

-}
stoppableFoldr : (v -> acc -> FastDict.Step acc) -> acc -> Set v -> acc
stoppableFoldr func acc (Set set) =
    FastDict.stoppableFoldr (\v _ iacc -> func v iacc) acc set
