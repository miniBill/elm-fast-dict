module Transform exposing (suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import Fuzzers exposing (Key, Value, dictFuzzer)
import Invariants exposing (expectDictRespectsInvariants)
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
                        , fuzz dictFuzzer "Respects the invariants" <|
                            \dict ->
                                dict
                                    |> Dict.map f
                                    |> expectDictRespectsInvariants
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
    describe "filter"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.filter >> fromList" <|
            \dict ->
                let
                    f : Key -> Value -> Bool
                    f _ v =
                        modBy 2 v == 0
                in
                dict
                    |> Dict.filter f
                    |> Expect.all
                        [ expectEqual
                            (dict
                                |> Dict.toList
                                |> List.filter (\( k, v ) -> f k v)
                                |> Dict.fromList
                            )
                        , expectDictRespectsInvariants
                        ]
        ]


partitionTest : Test
partitionTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 v == 0
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
        , describe "first"
            [ fuzz dictFuzzer "Respects the invariants" <|
                \dict ->
                    dict
                        |> Dict.partition f
                        |> Tuple.first
                        |> expectDictRespectsInvariants
            ]
        , describe "second"
            [ fuzz dictFuzzer "Respects the invariants" <|
                \dict ->
                    dict
                        |> Dict.partition f
                        |> Tuple.second
                        |> expectDictRespectsInvariants
            ]
        ]
