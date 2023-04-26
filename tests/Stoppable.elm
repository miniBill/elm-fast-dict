module Stoppable exposing (suite)

import Expect
import FastDict as Dict
import Fuzzers exposing (dictFuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "stoppable"
        [ stoppableFoldlTest
        , stoppableFoldrTest
        ]


stoppableFoldlTest : Test
stoppableFoldlTest =
    describe "stoppableFoldl"
        [ fuzz dictFuzzer "Behaves like foldl if we always Continue" <|
            \dict ->
                dict
                    |> Dict.stoppableFoldl (\k v acc -> Dict.Continue (( k, v ) :: acc)) []
                    |> Expect.equalLists (Dict.foldl (\k v acc -> ( k, v ) :: acc) [] dict)
        , fuzz dictFuzzer "Gets the lowest keys if we stop early" <|
            \dict ->
                dict
                    |> Dict.stoppableFoldl
                        (\k v acc ->
                            if k < "10" then
                                Dict.Continue (( k, v ) :: acc)

                            else
                                Dict.Stop acc
                        )
                        []
                    |> Expect.equalLists
                        (Dict.foldl
                            (\k v acc ->
                                if k < "10" then
                                    ( k, v ) :: acc

                                else
                                    acc
                            )
                            []
                            dict
                        )
        ]


stoppableFoldrTest : Test
stoppableFoldrTest =
    describe "stoppableFoldr"
        [ fuzz dictFuzzer "Behaves like foldr if we always Continue" <|
            \dict ->
                dict
                    |> Dict.stoppableFoldr (\k v acc -> Dict.Continue (( k, v ) :: acc)) []
                    |> Expect.equalLists (Dict.foldr (\k v acc -> ( k, v ) :: acc) [] dict)
        , fuzz dictFuzzer "Gets the highest keys if we stop early" <|
            \dict ->
                dict
                    |> Dict.stoppableFoldr
                        (\k v acc ->
                            if k > "10" then
                                Dict.Continue (( k, v ) :: acc)

                            else
                                Dict.Stop acc
                        )
                        []
                    |> Expect.equalLists
                        (Dict.foldr
                            (\k v acc ->
                                if k > "10" then
                                    ( k, v ) :: acc

                                else
                                    acc
                            )
                            []
                            dict
                        )
        ]
