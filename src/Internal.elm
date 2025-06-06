module Internal exposing (Dict(..), InnerDict(..), NColor(..), VisitQueue, balance, fromSortedList, insertNoReplace, unconsBiggest, unconsBiggestWhileDroppingGT)

import ListWithLength exposing (ListWithLength)



-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


type InnerDict k v
    = InnerNode NColor k v (InnerDict k v) (InnerDict k v)
    | Leaf


type Dict k v
    = Dict Int (InnerDict k v)


insertNoReplace : comparable -> v -> Dict comparable v -> Dict comparable v
insertNoReplace key value (Dict sz dict) =
    let
        ( result, isNew ) =
            insertInnerNoReplace key value dict
    in
    if isNew then
        Dict (sz + 1) result

    else
        Dict sz result


insertInnerNoReplace : comparable -> v -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
insertInnerNoReplace key value dict =
    -- Root node is always Black
    case insertHelpNoReplace key value dict of
        ( InnerNode Red k v l r, isNew ) ->
            ( InnerNode Black k v l r, isNew )

        x ->
            x


insertHelpNoReplace : comparable -> v -> InnerDict comparable v -> ( InnerDict comparable v, Bool )
insertHelpNoReplace key value dict =
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
                            insertHelpNoReplace key value nLeft
                    in
                    ( balance nColor nKey nValue newLeft nRight, isNew )

                EQ ->
                    ( dict, False )

                GT ->
                    let
                        ( newRight, isNew ) =
                            insertHelpNoReplace key value nRight
                    in
                    ( balance nColor nKey nValue nLeft newRight, isNew )


{-| Builds a Dict from an already sorted list.

WARNING: This does _not_ check that the list is sorted.

-}
fromSortedList : ListWithLength ( comparable, v ) -> Dict comparable v
fromSortedList dacc =
    let
        len : Int
        len =
            ListWithLength.length dacc

        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat len))

        go : Int -> Int -> Int -> List ( comparable, v ) -> ( InnerDict comparable v, List ( comparable, v ) )
        go layer fromIncluded toExcluded acc =
            if fromIncluded >= toExcluded then
                ( Leaf, acc )

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2

                    ( lchild, accAfterLeft ) =
                        go (layer + 1) fromIncluded mid acc
                in
                case accAfterLeft of
                    [] ->
                        ( Leaf, acc )

                    ( k, v ) :: tail ->
                        let
                            ( rchild, accAfterRight ) =
                                go (layer + 1) (mid + 1) toExcluded tail

                            color : NColor
                            color =
                                if layer > 0 && layer == redLayer then
                                    Red

                                else
                                    Black
                        in
                        ( InnerNode color k v lchild rchild
                        , accAfterRight
                        )
    in
    go 0 0 len (ListWithLength.toList dacc)
        |> Tuple.first
        |> Dict len


{-| This is a list of nodes that are going to be visited.
-}
type alias VisitQueue comparable v =
    List (InnerDict comparable v)


{-| Try getting the biggest key/value pair from the visit queue
-}
unconsBiggest : VisitQueue comparable v -> Maybe ( comparable, v, VisitQueue comparable v )
unconsBiggest queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                InnerNode _ key value Leaf Leaf ->
                    Just ( key, value, t )

                InnerNode _ key value childLT Leaf ->
                    Just ( key, value, childLT :: t )

                InnerNode color key value childLT childGT ->
                    unconsBiggest (childGT :: InnerNode color key value childLT Leaf :: t)

                Leaf ->
                    unconsBiggest t


{-| Try getting the biggest key/value pair from the visit queue, while dropping all values greater than the given key
-}
unconsBiggestWhileDroppingGT : comparable -> VisitQueue comparable v -> Maybe ( comparable, v, VisitQueue comparable v )
unconsBiggestWhileDroppingGT compareKey queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                InnerNode color key value childLT childGT ->
                    if key > compareKey then
                        unconsBiggestWhileDroppingGT compareKey (childLT :: t)

                    else if key == compareKey then
                        Just ( key, value, childLT :: t )

                    else
                        case childGT of
                            Leaf ->
                                Just ( key, value, childLT :: t )

                            _ ->
                                unconsBiggestWhileDroppingGT compareKey (childGT :: InnerNode color key value childLT Leaf :: t)

                Leaf ->
                    unconsBiggestWhileDroppingGT compareKey t


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
