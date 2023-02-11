module Internal exposing (Dict(..), InnerDict(..), NColor(..), fromSortedList)

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
