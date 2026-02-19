module Internal exposing (Dict(..), InnerDict(..), NColor(..), VisitQueue, balance, fromSortedList, innerNode, insertNoReplace, setRootBlack, unconsBiggest, unconsBiggestWhileDroppingGT)

import ListWithLength exposing (ListWithLength)



-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


type InnerDict k v
    = InnerNode NColor Int k v (InnerDict k v) (InnerDict k v)
    | Leaf


type Dict k v
    = Dict Int (InnerDict k v)


insertNoReplace : comparable -> v -> Dict comparable v -> Dict comparable v
insertNoReplace key value ((Dict sz dict) as orig) =
    case insertHelpNoReplace key value dict of
        Just result ->
            Dict (sz + 1) (setRootBlack result)

        Nothing ->
            orig


setRootBlack : InnerDict k v -> InnerDict k v
setRootBlack dict =
    case dict of
        InnerNode Red bh k v l r ->
            InnerNode Black (bh + 1) k v l r

        x ->
            x


insertHelpNoReplace : comparable -> v -> InnerDict comparable v -> Maybe (InnerDict comparable v)
insertHelpNoReplace key value dict =
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Just (InnerNode Red 1 key value Leaf Leaf)

        InnerNode nColor _ nKey nValue nLeft nRight ->
            case compare key nKey of
                LT ->
                    case insertHelpNoReplace key value nLeft of
                        Just newLeft ->
                            balance nColor nKey nValue newLeft nRight
                                |> Just

                        Nothing ->
                            Nothing

                EQ ->
                    Nothing

                GT ->
                    case insertHelpNoReplace key value nRight of
                        Just newRight ->
                            balance nColor nKey nValue nLeft newRight
                                |> Just

                        Nothing ->
                            Nothing


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
                        ( innerNode color k v lchild rchild
                        , accAfterRight
                        )
    in
    go 0 0 len (ListWithLength.toList dacc)
        |> Tuple.first
        |> Dict len


innerNode : NColor -> k -> v -> InnerDict k v -> InnerDict k v -> InnerDict k v
innerNode color k v left right =
    let
        childBlackHeight : Int
        childBlackHeight =
            case left of
                Leaf ->
                    1

                InnerNode _ bh _ _ _ _ ->
                    bh

        blackHeight : Int
        blackHeight =
            case color of
                Black ->
                    childBlackHeight + 1

                Red ->
                    childBlackHeight
    in
    InnerNode color blackHeight k v left right


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
                InnerNode _ _ key value Leaf Leaf ->
                    Just ( key, value, t )

                InnerNode _ _ key value childLT Leaf ->
                    Just ( key, value, childLT :: t )

                InnerNode color _ key value childLT childGT ->
                    unconsBiggest (childGT :: InnerNode color -1 key value childLT Leaf :: t)

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
                InnerNode color _ key value childLT childGT ->
                    if key > compareKey then
                        unconsBiggestWhileDroppingGT compareKey (childLT :: t)

                    else if key == compareKey then
                        Just ( key, value, childLT :: t )

                    else
                        case childGT of
                            Leaf ->
                                Just ( key, value, childLT :: t )

                            _ ->
                                unconsBiggestWhileDroppingGT compareKey (childGT :: InnerNode color -1 key value childLT Leaf :: t)

                Leaf ->
                    unconsBiggestWhileDroppingGT compareKey t


balance : NColor -> k -> v -> InnerDict k v -> InnerDict k v -> InnerDict k v
balance color key value left right =
    case right of
        InnerNode Red rBh rK rV rLeft rRight ->
            case left of
                InnerNode Red lBh lK lV lLeft lRight ->
                    let
                        bh : Int
                        bh =
                            lBh + 1
                    in
                    InnerNode
                        Red
                        bh
                        key
                        value
                        (InnerNode Black bh lK lV lLeft lRight)
                        (InnerNode Black bh rK rV rLeft rRight)

                _ ->
                    case color of
                        Black ->
                            InnerNode color (rBh + 1) rK rV (InnerNode Red rBh key value left rLeft) rRight

                        Red ->
                            InnerNode color rBh rK rV (InnerNode Red rBh key value left rLeft) rRight

        _ ->
            case left of
                InnerNode Red _ lK lV (InnerNode Red llBh llK llV llLeft llRight) lRight ->
                    let
                        bh : Int
                        bh =
                            llBh + 1
                    in
                    InnerNode
                        Red
                        bh
                        lK
                        lV
                        (InnerNode Black bh llK llV llLeft llRight)
                        (InnerNode Black bh key value lRight right)

                _ ->
                    let
                        rBh : Int
                        rBh =
                            case right of
                                InnerNode _ h _ _ _ _ ->
                                    h

                                Leaf ->
                                    1
                    in
                    case color of
                        Black ->
                            InnerNode color (rBh + 1) key value left right

                        Red ->
                            InnerNode color rBh key value left right
