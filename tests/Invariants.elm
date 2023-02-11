module Invariants exposing (blackHeight, hasCorrectSize, isBst, isRootBlack, noRedChildOfRedNode)

import Internal exposing (Dict(..), InnerDict(..), NColor(..))


hasCorrectSize : Dict comparable v -> Bool
hasCorrectSize (Dict sz dict) =
    let
        go : InnerDict k v -> Int
        go n =
            case n of
                Leaf ->
                    0

                InnerNode _ _ _ l r ->
                    1 + go l + go r
    in
    go dict == sz


isRootBlack : Dict comparable v -> Bool
isRootBlack (Dict _ dict) =
    case dict of
        Leaf ->
            True

        InnerNode color _ _ _ _ ->
            color == Black


blackHeight : Dict k v -> Maybe Int
blackHeight (Dict _ dict) =
    let
        go n =
            case n of
                Leaf ->
                    Just 1

                InnerNode color _ _ l r ->
                    let
                        local : Int
                        local =
                            case color of
                                Black ->
                                    1

                                Red ->
                                    0
                    in
                    case ( go l, go r ) of
                        ( Just lbh, Just rbh ) ->
                            if lbh == rbh then
                                Just (local + lbh)

                            else
                                Nothing

                        _ ->
                            Nothing
    in
    go dict


isBst : Dict comparable v -> Bool
isBst (Dict _ dict) =
    let
        go : Maybe comparable -> Maybe comparable -> InnerDict comparable v -> Bool
        go low high n =
            case n of
                Leaf ->
                    True

                InnerNode _ k _ l r ->
                    let
                        respectsLow : Bool
                        respectsLow =
                            case low of
                                Nothing ->
                                    True

                                Just lowV ->
                                    k > lowV

                        respectsHigh : Bool
                        respectsHigh =
                            case high of
                                Nothing ->
                                    True

                                Just highV ->
                                    k < highV
                    in
                    respectsLow && respectsHigh && go low (Just k) l && go (Just k) high r
    in
    go Nothing Nothing dict


noRedChildOfRedNode : Dict k v -> Bool
noRedChildOfRedNode (Dict _ dict) =
    let
        go n =
            case n of
                Leaf ->
                    True

                InnerNode Red _ _ (InnerNode Red _ _ _ _) _ ->
                    False

                InnerNode Red _ _ _ (InnerNode Red _ _ _ _) ->
                    False

                InnerNode _ _ _ l r ->
                    go l && go r
    in
    go dict
