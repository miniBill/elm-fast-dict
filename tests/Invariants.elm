module Invariants exposing (respectsInvariants, respectsInvariantsFuzz)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Internal exposing (Dict(..), InnerDict(..), NColor(..))
import Test exposing (Test, describe, fuzz, test)


{-| Checks whether a dictionary respects the five invariants:

1.  the root is black
2.  the cached size is the amount of inner nodes
3.  the tree is a BST
4.  the black height is equal on all branches
5.  no red node has a red child

-}
respectsInvariants : Dict comparable value -> Test
respectsInvariants dict =
    describe "Respects the invariants"
        [ test "The root is black" <|
            \_ ->
                dict
                    |> isRootBlack
                    |> Expect.equal True
        , test "The cached size is correct" <|
            \_ ->
                hasCorrectSize dict
        , test "It is a BST" <|
            \_ ->
                dict
                    |> isBst
                    |> Expect.equal True
        , test "The black height is consistent" <|
            \_ ->
                dict
                    |> blackHeight
                    |> Expect.notEqual Nothing
        , test "No red node has a red child" <|
            \_ ->
                dict
                    |> noRedChildOfRedNode
                    |> Expect.equal True
        ]


{-| Checks whether a dictionary respects the five invariants:

1.  the root is black
2.  the cached size is the amount of inner nodes
3.  the tree is a BST
4.  the black height is equal on all branches
5.  no red node has a red child

-}
respectsInvariantsFuzz : Fuzzer (Dict comparable value) -> Test
respectsInvariantsFuzz fuzzer =
    describe "Respects the invariants"
        [ fuzz fuzzer "The root is black" <|
            \dict ->
                dict
                    |> isRootBlack
                    |> Expect.equal True
        , fuzz fuzzer "The cached size is correct" <|
            \dict ->
                hasCorrectSize dict
        , fuzz fuzzer "It is a BST" <|
            \dict ->
                dict
                    |> isBst
                    |> Expect.equal True
        , fuzz fuzzer "The black height is consistent" <|
            \dict ->
                dict
                    |> blackHeight
                    |> Expect.notEqual Nothing
        , fuzz fuzzer "No red node has a red child" <|
            \dict ->
                dict
                    |> noRedChildOfRedNode
                    |> Expect.equal True
        ]


hasCorrectSize : Dict comparable v -> Expectation
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
    sz
        |> Expect.equal (go dict)


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
        go : InnerDict k v -> Maybe Int
        go n =
            case n of
                Leaf ->
                    Just 1

                InnerNode color _ _ l r ->
                    case ( go l, go r ) of
                        ( Just lbh, Just rbh ) ->
                            if lbh == rbh then
                                let
                                    local : Int
                                    local =
                                        case color of
                                            Black ->
                                                1

                                            Red ->
                                                0
                                in
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
        go : InnerDict k v -> Bool
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
