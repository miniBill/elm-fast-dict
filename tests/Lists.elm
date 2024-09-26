module Lists exposing (suite)

import Expect
import FastDict as Dict
import Fuzzers exposing (Key, dictFuzzer, pairListFuzzer)
import Invariants exposing (respectsInvariantsFuzz)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "lists"
        [ keysTest
        , valuesTest
        , toListTest
        , fromListTest
        ]


keysTest : Test
keysTest =
    describe "keys"
        [ fuzz dictFuzzer "Is equivalent to List.map Tuple.first toList" <|
            \dict ->
                dict
                    |> Dict.keys
                    |> Expect.equalLists (List.map Tuple.first (Dict.toList dict))
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.keys dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        , fuzz dictFuzzer "Is sorted" <|
            \dict ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (List.sort keys)
        , fuzz dictFuzzer "Contains no duplicates" <|
            \dict ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (dedupBy identity <| List.sort keys)
        ]


valuesTest : Test
valuesTest =
    describe "values"
        [ fuzz dictFuzzer "Is equivalent to List.map Tuple.second toList" <|
            \dict ->
                dict
                    |> Dict.values
                    |> Expect.equalLists (List.map Tuple.second (Dict.toList dict))
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.values dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


toListTest : Test
toListTest =
    describe "toList"
        [ fuzz dictFuzzer "Is sorted by key" <|
            \dict ->
                dict
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.toList dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


fromListTest : Test
fromListTest =
    describe "fromList"
        [ fuzz pairListFuzzer "Combined with toList is the equivalent of sort >> dedupBy Tuple.first" <|
            \list ->
                list
                    |> Dict.fromList
                    |> Dict.toList
                    |> Expect.equalLists (dedupBy Tuple.first (List.sortBy Tuple.first list))
        , fuzz dictFuzzer "Is the inverse to toList" <|
            \dict ->
                dict
                    |> Dict.toList
                    |> Dict.fromList
                    |> Dict.toList
                    |> Expect.equal (Dict.toList dict)

        -- , fuzz pairListFuzzer "Is equivalent to the fast version" <|
        --     \list ->
        --         list
        --             |> Dict.fromListFast
        --             |> Dict.toList
        --             |> Expect.equalLists
        --                 (list
        --                     |> Dict.fromList
        --                     |> Dict.toList
        --                 )
        , respectsInvariantsFuzz dictFuzzer
        ]


dedupBy : (a -> b) -> List a -> List a
dedupBy f =
    List.foldr
        (\e acc ->
            case acc of
                [] ->
                    [ e ]

                last :: _ ->
                    if f e == f last then
                        acc

                    else
                        e :: acc
        )
        []
