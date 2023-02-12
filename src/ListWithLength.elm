module ListWithLength exposing
    ( ListWithLength
    , empty, cons
    , length
    , toList
    )

{-| A list keeping track of its length.


# Types

@docs ListWithLength


# Build

@docs empty, cons


# Query

@docs length


# Convert

@docs toList

-}


type ListWithLength a
    = ListWithLength Int (List a)


{-| An empty list.
-}
empty : ListWithLength a
empty =
    ListWithLength 0 []


{-| Prepend an element to the list.

This function is O(1).

-}
cons : a -> ListWithLength a -> ListWithLength a
cons x (ListWithLength s xs) =
    ListWithLength (s + 1) (x :: xs)


{-| Get the list's length.

This function is O(1).

-}
length : ListWithLength a -> Int
length (ListWithLength s _) =
    s


{-| Get the list content.

This function is O(1).

-}
toList : ListWithLength a -> List a
toList (ListWithLength _ xs) =
    xs
