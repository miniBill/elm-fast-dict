module FastDict exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size, equals
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size, equals


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

{- Parts of this file (the documentation and API, and part of the implementation) are copied or adapted from `elm/core`, and thus they are Copyright 2014-present Evan Czaplicki -}
-- DICTIONARIES
-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type Dict k v
    = Dict Int (InnerDict k v)


type InnerDict k v
    = InnerNode NColor k v (InnerDict k v) (InnerDict k v)
    | Leaf


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 Leaf


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get targetKey (Dict _ dict) =
    getInner targetKey dict


getInner : comparable -> InnerDict comparable v -> Maybe v
getInner targetKey dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode _ key value left right ->
            case compare targetKey key of
                LT ->
                    getInner targetKey left

                EQ ->
                    Just value

                GT ->
                    getInner targetKey right


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size (Dict sz _) =
    sz


{-| Determine if two dictionaries are equal. This is needed because the structure could be different depending on insertion order.
-}
equals : Dict k v -> Dict k v -> Bool
equals ((Dict lsz _) as l) ((Dict rsz _) as r) =
    lsz == rsz && toList l == toList r


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty (Dict _ dict) =
    case dict of
        Leaf ->
            True

        InnerNode _ _ _ _ _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value (Dict sz dict) =
    let
        ( result, isNew ) =
            insertInner key value dict
    in
    if isNew then
        Dict (sz + 1) result

    else
        Dict sz result


insertInner : comparable -> v -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
insertInner key value dict =
    -- Root node is always Black
    case insertHelp key value dict of
        ( InnerNode Red k v l r, isNew ) ->
            ( InnerNode Black k v l r, isNew )

        x ->
            x


insertHelp : comparable -> v -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
insertHelp key value dict =
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            ( InnerNode Red key value Leaf Leaf, True )

        InnerNode nColor nKey nValue nLeft nRight ->
            case compare key nKey of
                LT ->
                    let
                        ( newLeft, isNew ) =
                            insertHelp key value nLeft
                    in
                    ( balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( InnerNode nColor nKey value nLeft nRight, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelp key value nRight
                    in
                    ( balance nColor nKey nValue nLeft newRight, isNew )


balance : NColor -> k -> v -> InnerDict k v -> InnerDict k v -> InnerDict k v
balance color key value left right =
    case right of
        InnerNode Red rK rV rLeft rRight ->
            case left of
                InnerNode Red lK lV lLeft lRight ->
                    InnerNode
                        Red
                        key
                        value
                        (InnerNode Black lK lV lLeft lRight)
                        (InnerNode Black rK rV rLeft rRight)

                _ ->
                    InnerNode color rK rV (InnerNode Red key value left rLeft) rRight

        _ ->
            case left of
                InnerNode Red lK lV (InnerNode Red llK llV llLeft llRight) lRight ->
                    InnerNode
                        Red
                        lK
                        lV
                        (InnerNode Black llK llV llLeft llRight)
                        (InnerNode Black key value lRight right)

                _ ->
                    InnerNode color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key ((Dict sz dict) as orig) =
    let
        ( result, wasMember ) =
            removeInner key dict
    in
    if wasMember then
        Dict (sz - 1) result

    else
        orig


removeInner : comparable -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
removeInner key dict =
    -- Root node is always Black
    case removeHelp key dict of
        ( InnerNode Red k v l r, wasMember ) ->
            ( InnerNode Black k v l r, wasMember )

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : comparable -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
removeHelp targetKey dict =
    case dict of
        Leaf ->
            ( Leaf, False )

        InnerNode color key value left right ->
            if targetKey < key then
                case left of
                    InnerNode Black _ _ lLeft _ ->
                        case lLeft of
                            InnerNode Red _ _ _ _ ->
                                let
                                    ( newLeft, wasMember ) =
                                        removeHelp targetKey left
                                in
                                ( InnerNode color key value newLeft right, wasMember )

                            _ ->
                                let
                                    res : { color : NColor, k : comparable, v : v, left : InnerDict comparable v, right : InnerDict comparable v }
                                    res =
                                        moveRedLeft color key value left right

                                    ( newLeft, wasMember ) =
                                        removeHelp targetKey res.left
                                in
                                ( balance res.color res.k res.v newLeft res.right, wasMember )

                    _ ->
                        let
                            ( newLeft, wasMember ) =
                                removeHelp targetKey left
                        in
                        ( InnerNode color key value newLeft right, wasMember )

            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT dict color key value left right)


removeHelpPrepEQGT : InnerDict comparable v -> NColor -> comparable -> v -> InnerDict comparable v -> InnerDict comparable v -> InnerDict comparable v
removeHelpPrepEQGT dict color key value left right =
    case left of
        InnerNode Red lK lV lLeft lRight ->
            InnerNode
                color
                lK
                lV
                lLeft
                (InnerNode Red key value lRight right)

        InnerNode Black lK lV lLeft lRight ->
            case right of
                InnerNode Black rK rV ((InnerNode Black _ _ _ _) as rLeft) rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight

                InnerNode Black rK rV Leaf rRight ->
                    moveRedRight key value lK lV lLeft lRight rK rV Leaf rRight

                _ ->
                    dict

        Leaf ->
            dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : comparable -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
removeHelpEQGT targetKey dict =
    case dict of
        InnerNode color key value left right ->
            if targetKey == key then
                case getMin right of
                    InnerNode _ minKey minValue _ _ ->
                        ( balance color minKey minValue left (removeMin right), True )

                    Leaf ->
                        ( Leaf, True )

            else
                let
                    ( newRight, wasMember ) =
                        removeHelp targetKey right
                in
                ( balance color key value left newRight, wasMember )

        Leaf ->
            ( Leaf, False )


getMin : InnerDict k v -> InnerDict k v
getMin dict =
    case dict of
        InnerNode _ _ _ ((InnerNode _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : InnerDict k v -> InnerDict k v
removeMin dict =
    case dict of
        InnerNode color key value ((InnerNode lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        InnerNode Red _ _ _ _ ->
                            InnerNode color key value (removeMin left) right

                        _ ->
                            let
                                res : { color : NColor, k : k, v : v, left : InnerDict k v, right : InnerDict k v }
                                res =
                                    moveRedLeft color key value left right
                            in
                            balance res.color res.k res.v (removeMin res.left) res.right

                _ ->
                    InnerNode color key value (removeMin left) right

        _ ->
            Leaf


moveRedLeft : NColor -> k -> v -> InnerDict k v -> InnerDict k v -> { color : NColor, k : k, v : v, left : InnerDict k v, right : InnerDict k v }
moveRedLeft clr k v left right =
    case left of
        InnerNode _ lK lV lLeft lRight ->
            case right of
                InnerNode _ rK rV (InnerNode Red rlK rlV rlL rlR) rRight ->
                    { color = Red
                    , k = rlK
                    , v = rlV
                    , left = InnerNode Black k v (InnerNode Red lK lV lLeft lRight) rlL
                    , right = InnerNode Black rK rV rlR rRight
                    }

                InnerNode _ rK rV rLeft rRight ->
                    case clr of
                        Black ->
                            { color = Black
                            , k = k
                            , v = v
                            , left = InnerNode Red lK lV lLeft lRight
                            , right = InnerNode Red rK rV rLeft rRight
                            }

                        Red ->
                            { color = Black
                            , k = k
                            , v = v
                            , left = InnerNode Red lK lV lLeft lRight
                            , right = InnerNode Red rK rV rLeft rRight
                            }

                _ ->
                    { color = clr, k = k, v = v, left = left, right = right }

        _ ->
            { color = clr, k = k, v = v, left = left, right = right }


moveRedRight : k -> v -> k -> v -> InnerDict k v -> InnerDict k v -> k -> v -> InnerDict k v -> InnerDict k v -> InnerDict k v
moveRedRight key value lK lV lLeft lRight rK rV rLeft rRight =
    case lLeft of
        InnerNode Red llK llV llLeft llRight ->
            InnerNode
                Red
                lK
                lV
                (InnerNode Black llK llV llLeft llRight)
                (InnerNode Black key value lRight (InnerNode Red rK rV rLeft rRight))

        _ ->
            InnerNode
                Black
                key
                value
                (InnerNode Red lK lV lLeft lRight)
                (InnerNode Red rK rV rLeft rRight)


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update targetKey alter dictionary =
    case alter (get targetKey dictionary) of
        Just value ->
            insert targetKey value dictionary

        Nothing ->
            remove targetKey dictionary


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    -- Root node is always Black
    Dict 1 (InnerNode Black key value Leaf Leaf)



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable a -> Dict comparable b -> Dict comparable a
diff t1 t2 =
    foldl (\k _ t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )

                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )

                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func (Dict sz dict) =
    Dict sz (mapInner func dict)


mapInner : (k -> a -> b) -> InnerDict k a -> InnerDict k b
mapInner func dict =
    case dict of
        Leaf ->
            Leaf

        InnerNode color key value left right ->
            InnerNode color key (func key value) (mapInner func left) (mapInner func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl func acc (Dict _ dict) =
    foldlInner func acc dict


foldlInner : (k -> v -> b -> b) -> b -> InnerDict k v -> b
foldlInner func acc dict =
    case dict of
        Leaf ->
            acc

        InnerNode _ key value left right ->
            foldlInner func (func key value (foldlInner func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr func acc (Dict _ dict) =
    foldrInner func acc dict


foldrInner : (k -> v -> b -> b) -> b -> InnerDict k v -> b
foldrInner func acc t =
    case t of
        Leaf ->
            acc

        InnerNode _ key value left right ->
            foldrInner func (func key value (foldrInner func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter isGood dict =
    foldl
        (\k v d ->
            if isGood k v then
                insert k v d

            else
                d
        )
        empty
        dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition isGood dict =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : Dict k v -> List k
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : Dict k v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs
