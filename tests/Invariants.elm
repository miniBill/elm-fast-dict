module Invariants exposing (expectDictRespectsInvariants, hasCorrectSize, respectsInvariantsFuzz)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Internal exposing (Dict(..), InnerDict(..), NColor(..))
import Test exposing (Test, fuzz)
import Test.Runner


{-| Checks whether a dictionary respects the five invariants:

1.  the root is black
2.  the cached size is the amount of inner nodes
3.  the tree is a BST
4.  the black height is equal on all branches
5.  no red node has a red child

-}
respectsInvariantsFuzz : (a -> Dict comparable value) -> Fuzzer a -> Test
respectsInvariantsFuzz f fuzzer =
    fuzz fuzzer "Respects the invariants" <|
        \dict ->
            expectDictRespectsInvariants (f dict)


{-| Checks whether a dictionary respects the five invariants:

1.  the root is black
2.  the cached size is the amount of inner nodes
3.  the tree is a BST
4.  the black height is equal on all branches
5.  no red node has a red child

-}
expectDictRespectsInvariants : Dict comparable v -> Expectation
expectDictRespectsInvariants dict =
    Expect.all
        [ \() ->
            dict
                |> isRootBlack
                |> Expect.equal True
                |> explainError dict "The root is not black"
        , \() ->
            hasCorrectSize dict
        , \() ->
            dict
                |> isBst
                |> Expect.equal True
                |> explainError dict "The Dict is not a BST"
        , \() ->
            dict
                |> blackHeight
                |> Expect.notEqual Nothing
                |> explainError dict "The black height is not consistent"
        , \() ->
            dict
                |> noRedChildOfRedNode
                |> Expect.equal True
                |> explainError dict "A red node has a red child"
        ]
        ()


explainError : Dict k v -> String -> Expectation -> Expectation
explainError (Dict size dict) prefix expectation =
    expectation
        |> onFail (\() -> prefix ++ ":\n\nSize: " ++ String.fromInt size ++ "\n" ++ printTree 0 dict "")


printTree : Int -> InnerDict k v -> String -> String
printTree level dict acc =
    case dict of
        Leaf ->
            acc

        InnerNode color key value left right ->
            printTree (level + 1)
                right
                (printTree (level + 1)
                    left
                    (acc
                        ++ "\n"
                        ++ String.repeat level "    "
                        ++ String.join " "
                            [ "↳"
                            , colorToString color
                            , Debug.toString key ++ "->" ++ Debug.toString value
                            ]
                    )
                )


colorToString : NColor -> String
colorToString color =
    case color of
        Red ->
            "R"

        Black ->
            "B"


onFail : (() -> String) -> Expectation -> Expectation
onFail message expectation =
    case Test.Runner.getFailureReason expectation of
        Just _ ->
            expectation |> Expect.onFail (message ())

        Nothing ->
            expectation


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
