module Transform exposing (suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import Fuzz exposing (Fuzzer)
import Fuzzers exposing (Key, Value, dictFuzzer)
import Internal exposing (Dict)
import Invariants exposing (respectsInvariantsFuzz)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "transform"
        [ mapTest
        , foldlTest
        , foldrTest
        , filterTest
        , partitionTest
        ]


mapTest : Test
mapTest =
    let
        f1 : String -> Int -> String
        f1 k v =
            k ++ " " ++ String.fromInt v

        f2 : a -> Int -> String
        f2 _ v =
            String.fromInt <| v + 1

        tests : List Test
        tests =
            [ ( "f1", f1 )
            , ( "f2", f2 )
            ]
                |> List.map
                    (\( flabel, f ) ->
                        [ fuzz dictFuzzer "Is equivalent to mapping on the list" <|
                            \dict ->
                                dict
                                    |> Dict.map f
                                    |> expectEqual
                                        (dict
                                            |> Dict.toList
                                            |> List.map (\( k, v ) -> ( k, f k v ))
                                            |> Dict.fromList
                                        )
                        , fuzz dictFuzzer "Doesn't change the size" <|
                            \dict ->
                                dict
                                    |> Dict.map f
                                    |> Dict.size
                                    |> Expect.equal (Dict.size dict)
                        , respectsInvariantsFuzz (Fuzz.map (Dict.map f) dictFuzzer)
                        ]
                            |> describe flabel
                    )
    in
    describe "map"
        (tests
            ++ [ fuzz dictFuzzer "map (always identity) == identity" <|
                    \dict ->
                        dict
                            |> Dict.map (always identity)
                            |> expectEqual dict
               ]
        )


foldlTest : Test
foldlTest =
    describe "foldl"
        [ fuzz dictFuzzer "foldl (::) is equivalent to toList >> reverse" <|
            \dict ->
                dict
                    |> Dict.foldl (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (List.reverse <| Dict.toList dict)
        , fuzz dictFuzzer "foldl insert is an identity" <|
            \dict ->
                dict
                    |> Dict.foldl Dict.insert Dict.empty
                    |> expectEqual dict
        ]


foldrTest : Test
foldrTest =
    describe "foldr"
        [ fuzz dictFuzzer "foldr (::) is equivalent to toList" <|
            \dict ->
                dict
                    |> Dict.foldr (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictFuzzer "foldr insert is an identity" <|
            \dict ->
                dict
                    |> Dict.foldr Dict.insert Dict.empty
                    |> expectEqual dict
        ]


filterTest : Test
filterTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 v == 0

        filteredFuzzer : Fuzzer (Dict Key Value)
        filteredFuzzer =
            Fuzz.map (Dict.filter f) dictFuzzer
    in
    describe "filter"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.filter >> fromList" <|
            \dict ->
                dict
                    |> Dict.filter f
                    |> expectEqual
                        (dict
                            |> Dict.toList
                            |> List.filter (\( k, v ) -> f k v)
                            |> Dict.fromList
                        )
        , respectsInvariantsFuzz filteredFuzzer
        ]


partitionTest : Test
partitionTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 v == 0

        partitionedFuzzer : Fuzzer ( Dict Key Value, Dict Key Value )
        partitionedFuzzer =
            Fuzz.map (Dict.partition f) dictFuzzer
    in
    describe "partition"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.partition >> fromList" <|
            \dict ->
                let
                    ( l, r ) =
                        Dict.partition f dict

                    ( el, er ) =
                        dict
                            |> Dict.toList
                            |> List.partition (\( k, v ) -> f k v)
                            |> Tuple.mapBoth Dict.fromList Dict.fromList
                in
                Expect.all
                    [ \_ -> expectEqual el l
                    , \_ -> expectEqual er r
                    ]
                    ()
        , describe "first" [ respectsInvariantsFuzz (Fuzz.map Tuple.first partitionedFuzzer) ]
        , describe "second" [ respectsInvariantsFuzz (Fuzz.map Tuple.second partitionedFuzzer) ]
        ]
