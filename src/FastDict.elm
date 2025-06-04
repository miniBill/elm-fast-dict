module FastDict exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size, equals
    , getMinKey, getMin, getMaxKey, getMax
    , popMin, popMax
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , toCoreDict, fromCoreDict
    , Step(..), stoppableFoldl, stoppableFoldr, restructure
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


# Min / Max

@docs getMinKey, getMin, getMaxKey, getMax

@docs popMin, popMax


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Interoperability

@docs toCoreDict, fromCoreDict


# Advanced functions

@docs Step, stoppableFoldl, stoppableFoldr, restructure

-}

import Dict
import Internal exposing (Dict(..), InnerDict(..), NColor(..), VisitQueue)
import ListWithLength exposing (ListWithLength)



{- Parts of this file (the documentation and API, and much of the implementation) are copied or adapted from `elm/core`, and thus they are Copyright 2014-present Evan Czaplicki -}
-- DICTIONARIES


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import FastDict as Dict exposing (Dict)

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
type alias Dict k v =
    Internal.Dict k v


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 Leaf


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals : Dict String String
    animals =
        fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]

    get "Tom"   animals
    --> Just "Cat"

    get "Jerry" animals
    --> Just "Mouse"

    get "Spike" animals
    --> Nothing

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
equals : Dict comparable v -> Dict comparable v -> Bool
equals (Dict lsz lRoot) (Dict rsz rRoot) =
    let
        go : Maybe ( comparable, v, VisitQueue comparable v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> Bool
        go lList rList =
            case lList of
                Nothing ->
                    rList == Nothing

                Just ( lk, lv, lRest ) ->
                    case rList of
                        Nothing ->
                            False

                        Just ( rk, rv, rRest ) ->
                            lk == rk && lv == rv && go (Internal.unconsBiggest lRest) (Internal.unconsBiggest rRest)
    in
    lsz == rsz && go (Internal.unconsBiggest [ lRoot ]) (Internal.unconsBiggest [ rRoot ])


{-| Gets the smallest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMinKey
    --> Just 1


    empty
        |> getMinKey
    --> Nothing

-}
getMinKey : Dict k v -> Maybe k
getMinKey (Dict _ dict) =
    let
        go : InnerDict k v -> Maybe k
        go n =
            case n of
                Leaf ->
                    Nothing

                InnerNode _ k _ Leaf _ ->
                    Just k

                InnerNode _ _ _ l _ ->
                    go l
    in
    go dict


{-| Gets the biggest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMaxKey
    --> Just 2


    empty
        |> getMaxKey
    --> Nothing

-}
getMaxKey : Dict k v -> Maybe k
getMaxKey (Dict _ dict) =
    let
        go : InnerDict k v -> Maybe k
        go n =
            case n of
                Leaf ->
                    Nothing

                InnerNode _ k _ _ Leaf ->
                    Just k

                InnerNode _ _ _ _ r ->
                    go r
    in
    go dict


{-| Gets the key-value pair with the smallest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMin
    --> Just ( 1, 'z' )


    empty
        |> getMin
    --> Nothing

-}
getMin : Dict k v -> Maybe ( k, v )
getMin (Dict _ dict) =
    getMinInner dict


getMinInner : InnerDict k v -> Maybe ( k, v )
getMinInner n =
    case n of
        Leaf ->
            Nothing

        InnerNode _ k v Leaf _ ->
            Just ( k, v )

        InnerNode _ _ _ l _ ->
            getMinInner l


{-| Gets the key-value pair with the biggest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMax
    --> Just ( 2, 'a' )


    empty
        |> getMax
    --> Nothing

-}
getMax : Dict k v -> Maybe ( k, v )
getMax (Dict _ dict) =
    let
        go : InnerDict k v -> Maybe ( k, v )
        go n =
            case n of
                Leaf ->
                    Nothing

                InnerNode _ k v _ Leaf ->
                    Just ( k, v )

                InnerNode _ _ _ _ r ->
                    go r
    in
    go dict


{-| Removes the key-value pair with the smallest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMin
    --> Just ( ( 1, 'z' ), fromList [ ( 2, 'a' ) ] )


    empty
        |> popMin
    --> Nothing

-}
popMin : Dict comparable v -> Maybe ( ( comparable, v ), Dict comparable v )
popMin dict =
    -- TODO: make faster by adapting `remove`
    Maybe.map
        (\(( k, _ ) as kv) ->
            ( kv, remove k dict )
        )
        (getMin dict)


{-| Removes the key-value pair with the biggest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMax
    --> Just ( ( 2, 'a' ), fromList [ ( 1, 'z' ) ] )


    empty
        |> popMax
    --> Nothing

-}
popMax : Dict comparable v -> Maybe ( ( comparable, v ), Dict comparable v )
popMax dict =
    -- TODO: make faster by adapting `remove`
    Maybe.map
        (\(( k, _ ) as kv) ->
            ( kv, remove k dict )
        )
        (getMax dict)


{-| Determine if a dictionary is empty.

    isEmpty empty
    --> True

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
    insertHelp key value dict
        |> Tuple.mapFirst Internal.setRootBlack


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
                    ( Internal.balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( InnerNode nColor nKey value nLeft nRight, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelp key value nRight
                    in
                    ( Internal.balance nColor nKey nValue nLeft newRight, isNew )


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key ((Dict sz dict) as orig) =
    case removeInner key dict of
        Just result ->
            Dict (sz - 1) result

        Nothing ->
            orig


removeInner : comparable -> InnerDict comparable v -> Maybe (InnerDict comparable v)
removeInner key dict =
    -- Root node is always Black
    case removeHelp key dict of
        Just (InnerNode Red k v l r) ->
            Just (InnerNode Black k v l r)

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : comparable -> InnerDict comparable v -> Maybe (InnerDict comparable v)
removeHelp targetKey dict =
    case dict of
        Leaf ->
            Nothing

        InnerNode color key value left right ->
            if targetKey < key then
                case left of
                    InnerNode Black _ _ lLeft _ ->
                        case lLeft of
                            InnerNode Red _ _ _ _ ->
                                case removeHelp targetKey left of
                                    Just newLeft ->
                                        Just (InnerNode color key value newLeft right)

                                    Nothing ->
                                        Nothing

                            _ ->
                                let
                                    res : { color : NColor, k : comparable, v : v, left : InnerDict comparable v, right : InnerDict comparable v }
                                    res =
                                        moveRedLeft color key value left right
                                in
                                case removeHelp targetKey res.left of
                                    Just newLeft ->
                                        Just (Internal.balance res.color res.k res.v newLeft res.right)

                                    Nothing ->
                                        Nothing

                    _ ->
                        case removeHelp targetKey left of
                            Just newLeft ->
                                Just (InnerNode color key value newLeft right)

                            Nothing ->
                                Nothing

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
removeHelpEQGT : comparable -> InnerDict comparable v -> Maybe (InnerDict comparable v)
removeHelpEQGT targetKey dict =
    case dict of
        InnerNode color key value left right ->
            if targetKey == key then
                case getMinInner right of
                    Just ( minKey, minValue ) ->
                        Just (Internal.balance color minKey minValue left (removeMin right))

                    Nothing ->
                        Just Leaf

            else
                case removeHelp targetKey right of
                    Just newRight ->
                        Just (Internal.balance color key value left newRight)

                    Nothing ->
                        Nothing

        Leaf ->
            Nothing


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
                            Internal.balance res.color res.k res.v (removeMin res.left) res.right

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
union ((Dict s1 _) as t1) ((Dict s2 _) as t2) =
    -- -- TODO: Find a data-based heuristic instead of the vibe-based "2 *"
    -- if s1 > 2 * s2 then
    --     foldl insertNoReplace t1 t2
    -- else if s2 > 2 * s1 then
    --     foldl insert t2 t1
    -- else
    --     Union.union t1 t2
    if s1 > s2 then
        foldl Internal.insertNoReplace t1 t2

    else
        foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect (Dict sz1 t1) (Dict sz2 t2) =
    if sz1 == 0 || sz2 == 0 then
        empty

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        intersectFromZipper
            ListWithLength.empty
            (Internal.unconsBiggest [ t1 ])
            (Internal.unconsBiggest [ t2 ])
            |> Internal.fromSortedList


intersectFromZipper : ListWithLength ( comparable, v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> ListWithLength ( comparable, v )
intersectFromZipper dacc lleft rleft =
    case lleft of
        Nothing ->
            dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    dacc

                Just ( rkey, _, rtail ) ->
                    if lkey > rkey then
                        intersectFromZipper dacc (Internal.unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if rkey > lkey then
                        intersectFromZipper dacc lleft (Internal.unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (Internal.unconsBiggest ltail) (Internal.unconsBiggest rtail)


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable a -> Dict comparable b -> Dict comparable a
diff ((Dict sz1 _) as t1) t2 =
    if sz1 == 0 then
        empty

    else
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
        stepState : comparable -> b -> ( List ( comparable, a ), result ) -> ( List ( comparable, a ), result )
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

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldl addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from lowest to highest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from highest to lowest.

    getAges users
    --> [ 33, 19, 28 ]

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

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldr addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from highest to lowest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from lowest to highest.

    getAges users
    --> [ 28, 19, 33 ]

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
        add : comparable -> v -> ( Dict comparable v, Dict comparable v ) -> ( Dict comparable v, Dict comparable v )
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ 0, 1 ]

-}
keys : Dict k v -> List k
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ "Alice", "Bob" ]

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


{-| Convert an association list into a dictionary.
-}
fromListFast : List ( comparable, v ) -> Dict comparable v
fromListFast assocs =
    let
        dedup : List ( comparable, v ) -> ListWithLength ( comparable, v )
        dedup xs =
            case xs of
                [] ->
                    ListWithLength.empty

                head :: tail ->
                    dedupHelp head tail ListWithLength.empty

        dedupHelp : ( comparable, v ) -> List ( comparable, v ) -> ListWithLength ( comparable, v ) -> ListWithLength ( comparable, v )
        dedupHelp (( lastKey, _ ) as last) todo acc =
            case todo of
                [] ->
                    ListWithLength.cons last acc

                (( todoHeadKey, _ ) as todoHead) :: todoTail ->
                    let
                        newAcc : ListWithLength ( comparable, v )
                        newAcc =
                            if todoHeadKey == lastKey then
                                acc

                            else
                                ListWithLength.cons last acc
                    in
                    dedupHelp todoHead todoTail newAcc
    in
    assocs
        -- Intentionall swap k1 and k2 here to have a reverse sort so we can do dedup in one pass
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> compare k2 k1)
        |> dedup
        |> Internal.fromSortedList



-- INTEROPERABILITY


{-| Convert the dictionary into an equivalent one from elm/core.
-}
toCoreDict : Dict comparable v -> Dict.Dict comparable v
toCoreDict dict =
    foldl Dict.insert Dict.empty dict


{-| Convert the dictionary from an equivalent one from elm/core.
-}
fromCoreDict : Dict.Dict comparable v -> Dict comparable v
fromCoreDict dict =
    Dict.foldl insert empty dict



-- ADVANCED


{-| A custom type used for stoppable folds.
-}
type Step a
    = Continue a
    | Stop a


{-| A foldl that can stop early instead of traversing the whole dictionary.

    stoppableFoldl
        (\k v acc ->
            if k >= 10 then
                Stop acc
            else
                Continue (v + acc)
        )
        0
        (fromList <| List.indexedMap Tuple.pair <| List.range 1 10000)
    --> 55

-}
stoppableFoldl : (k -> v -> acc -> Step acc) -> acc -> Dict k v -> acc
stoppableFoldl func acc (Dict _ dict) =
    case stoppableFoldlInner func acc dict of
        Continue res ->
            res

        Stop res ->
            res


stoppableFoldlInner : (k -> v -> acc -> Step acc) -> acc -> InnerDict k v -> Step acc
stoppableFoldlInner func acc dict =
    case dict of
        Leaf ->
            Continue acc

        InnerNode _ key value left right ->
            case stoppableFoldlInner func acc left of
                Continue lacc ->
                    case func key value lacc of
                        Continue vacc ->
                            stoppableFoldlInner func vacc right

                        Stop vacc ->
                            Stop vacc

                Stop lacc ->
                    Stop lacc


{-| A foldr that can stop early instead of traversing the whole dictionary.

    stoppableFoldr
        (\k v acc ->
            if k <= 9990 then
                Stop acc
            else
                Continue (v + acc)
        )
        0
        (fromList <| List.indexedMap Tuple.pair <| List.range 1 10000)
    --> 89964

-}
stoppableFoldr : (k -> v -> acc -> Step acc) -> acc -> Dict k v -> acc
stoppableFoldr func acc (Dict _ dict) =
    case stoppableFoldrInner func acc dict of
        Continue res ->
            res

        Stop res ->
            res


stoppableFoldrInner : (k -> v -> acc -> Step acc) -> acc -> InnerDict k v -> Step acc
stoppableFoldrInner func acc dict =
    case dict of
        Leaf ->
            Continue acc

        InnerNode _ key value left right ->
            case stoppableFoldrInner func acc right of
                Continue racc ->
                    case func key value racc of
                        Continue vacc ->
                            stoppableFoldrInner func vacc left

                        Stop vacc ->
                            Stop vacc

                Stop lacc ->
                    Stop lacc


{-| This allows you to take advantage of the tree structure of the dictionary to do some operations more efficiently.

Calling `left` will give the result of calling `restructure` on the left subtree (lower keys), `right` on the right one (higher keys).

If this is confusing you probably don't need this function!

    any dict =
        -- Notice how if `value` is `True` we don't call `left` nor `right`,
        -- and if `value` is `False` but `left ()` is `True` we don't call right.
        restructure False (\{ value, left, right } -> value || left () || right ())

-}
restructure :
    acc
    -> ({ key : key, value : value, left : () -> acc, right : () -> acc } -> acc)
    -> Dict key value
    -> acc
restructure leafFunc nodeFunc (Dict _ dict) =
    restructureInner leafFunc nodeFunc dict


restructureInner : acc -> ({ key : key, value : value, left : () -> acc, right : () -> acc } -> acc) -> InnerDict key value -> acc
restructureInner leafFunc nodeFunc dict =
    case dict of
        Leaf ->
            leafFunc

        InnerNode _ key value left right ->
            nodeFunc
                { key = key
                , value = value
                , left = \() -> restructureInner leafFunc nodeFunc left
                , right = \() -> restructureInner leafFunc nodeFunc right
                }
