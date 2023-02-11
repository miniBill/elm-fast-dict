module Internal exposing (Dict(..), InnerDict(..), NColor(..), VisitQueue, fromSortedList, unconsBiggest, unconsBiggestWhileDroppingGT)

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
