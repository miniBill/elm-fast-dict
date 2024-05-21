module MinMax exposing (suite)

import Expect
import FastDict as Dict
import Fuzzers exposing (dictFuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "min/max"
        [ getMinKeyTest
        , getMinTest
        , getMaxKeyTest
        , getMaxTest
        , popMinTest
        , popMaxTest
        ]


getMinKeyTest : Test
getMinKeyTest =
    describe "getMinKey"
        [ fuzz dictFuzzer "Gets the smallest key" <|
            \dict ->
                dict
                    |> Dict.getMinKey
                    |> Expect.equal (List.head <| Dict.keys dict)
        ]


getMinTest : Test
getMinTest =
    describe "getMin"
        [ fuzz dictFuzzer "Gets the key-value pair with the smallest key" <|
            \dict ->
                dict
                    |> Dict.getMin
                    |> Expect.equal (List.head <| Dict.toList dict)
        ]


getMaxKeyTest : Test
getMaxKeyTest =
    describe "getMaxKey"
        [ fuzz dictFuzzer "Gets the biggest key" <|
            \dict ->
                dict
                    |> Dict.getMaxKey
                    |> Expect.equal (List.head <| List.reverse <| Dict.keys dict)
        ]


getMaxTest : Test
getMaxTest =
    describe "getMax"
        [ fuzz dictFuzzer "Gets the key-value pair with the biggest key" <|
            \dict ->
                dict
                    |> Dict.getMax
                    |> Expect.equal (List.head <| List.reverse <| Dict.toList dict)
        ]


popMinTest : Test
popMinTest =
    describe "popMin"
        [ fuzz dictFuzzer "Pops the key-value pair with the smallest key" <|
            \dict ->
                -- This test is currently useless, as it just copies the implementation,
                -- but it will be needed for the optimization effort.
                dict
                    |> Dict.popMin
                    |> Expect.equal
                        (Maybe.map
                            (\(( k, _ ) as kv) ->
                                ( kv, Dict.remove k dict )
                            )
                            (Dict.getMin dict)
                        )
        ]


popMaxTest : Test
popMaxTest =
    describe "popMax"
        [ fuzz dictFuzzer "Pops the key-value pair with the biggest key" <|
            \dict ->
                -- This test is currently useless, as it just copies the implementation,
                -- but it will be needed for the optimization effort.
                dict
                    |> Dict.popMax
                    |> Expect.equal
                        (Maybe.map
                            (\(( k, _ ) as kv) ->
                                ( kv, Dict.remove k dict )
                            )
                            (Dict.getMax dict)
                        )
        ]
