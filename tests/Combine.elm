module Combine exposing (suite)

import Expect
import FastDict as Dict exposing (Dict)
import Fuzz
import Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer)
import Invariants exposing (respectsInvariantsFuzz)
import Test exposing (Test, describe, fuzz2, fuzz3)


suite : Test
suite =
    describe "combine"
        [ unionTest
        , intersectTest
        , diffTest
        , mergeTest
        ]


unionTest : Test
unionTest =
    let
        unionFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        unionFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

        unionedFuzzer : Fuzz.Fuzzer (Dict Key Value)
        unionedFuzzer =
            Fuzz.map2 Dict.union dictFuzzer dictFuzzer
    in
    describe "union"
        [ fuzz2 unionFuzzer keyFuzzer "Contains the correct values giving preference to the first" <|
            \( first, second ) key ->
                case
                    ( Dict.get key first
                    , Dict.get key second
                    , Dict.get key (Dict.union first second)
                    )
                of
                    ( Just fvalue, _, Just uvalue ) ->
                        uvalue |> Expect.equal fvalue

                    ( Nothing, Just svalue, Just uvalue ) ->
                        uvalue |> Expect.equal svalue

                    ( Nothing, Nothing, Nothing ) ->
                        Expect.pass

                    ( Just _, _, Nothing ) ->
                        Expect.fail "Value found in first but not in union"

                    ( _, Just _, Nothing ) ->
                        Expect.fail "Value found in second but not in union"

                    ( Nothing, Nothing, Just _ ) ->
                        Expect.fail "Value found in union but not in first nor second"

        -- , fuzz2 unionFuzzer keyFuzzer "Contains the correct keys" <|
        --     \( first, second ) key ->
        --         Dict.member key (Dict.union first second)
        --             |> Expect.equal (Dict.member key first || Dict.member key second)
        , respectsInvariantsFuzz unionedFuzzer
        ]


intersectTest : Test
intersectTest =
    let
        intersectFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        intersectFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

        intersectedFuzzer : Fuzz.Fuzzer (Dict Key Value)
        intersectedFuzzer =
            Fuzz.map2 Dict.intersect dictFuzzer dictFuzzer
    in
    describe "intersect"
        [ fuzz2 intersectFuzzer keyFuzzer "Contains the correct values giving preference to the first" <|
            \( first, second ) key ->
                case
                    ( Dict.get key first
                    , Dict.get key second
                    , Dict.get key (Dict.intersect first second)
                    )
                of
                    ( Just fvalue, Just _, Just uvalue ) ->
                        uvalue |> Expect.equal fvalue

                    ( Nothing, _, Nothing ) ->
                        Expect.pass

                    ( _, Nothing, Nothing ) ->
                        Expect.pass

                    ( Nothing, _, Just _ ) ->
                        Expect.fail "Value found in intersection but not in first"

                    ( _, Nothing, Just _ ) ->
                        Expect.fail "Value found in intersection but not in second"

                    ( Just _, Just _, Nothing ) ->
                        Expect.fail "Value found in both but not in intersection"

        -- , fuzz2 intersectFuzzer keyFuzzer "Contains the correct keys" <|
        --     \( first, second ) key ->
        --         Dict.member key (Dict.intersect first second)
        --             |> Expect.equal (Dict.member key first && Dict.member key second)
        , respectsInvariantsFuzz intersectedFuzzer
        ]


diffTest : Test
diffTest =
    let
        diffFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        diffFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

        diffedFuzzer : Fuzz.Fuzzer (Dict Key Value)
        diffedFuzzer =
            Fuzz.map2 Dict.diff dictFuzzer dictFuzzer
    in
    describe "diff"
        [ fuzz2 diffFuzzer keyFuzzer "Contains the correct values giving preference to the first" <|
            \( first, second ) key ->
                let
                    diff : Dict Key Value
                    diff =
                        Dict.diff first second

                    got : Maybe Value
                    got =
                        Dict.get key diff
                in
                if got == Nothing then
                    -- This is checked in the other test
                    Expect.pass

                else
                    got
                        |> Expect.equal (Dict.get key first)
        , fuzz2 diffFuzzer keyFuzzer "Contains the correct keys" <|
            \( first, second ) key ->
                Dict.member key (Dict.diff first second)
                    |> Expect.equal (Dict.member key first && not (Dict.member key second))
        , respectsInvariantsFuzz diffedFuzzer
        ]


mergeTest : Test
mergeTest =
    describe "merge"
        [ fuzz3 dictFuzzer dictFuzzer keyFuzzer "Correctly categorizes elements" <|
            \left right key ->
                let
                    ( mergedL, mergedB, mergedR ) =
                        Dict.merge
                            (\lk lv ( l, b, r ) -> ( ( lk, lv ) :: l, b, r ))
                            (\bk lv rv ( l, b, r ) -> ( l, ( bk, lv, rv ) :: b, r ))
                            (\rk rv ( l, b, r ) -> ( l, b, ( rk, rv ) :: r ))
                            left
                            right
                            ( [], [], [] )

                    ( lMember, rMember ) =
                        ( Dict.member key left, Dict.member key right )
                in
                if Dict.isEmpty left && Dict.isEmpty right then
                    Expect.all
                        [ \_ -> List.isEmpty mergedL |> Expect.equal True
                        , \_ -> List.isEmpty mergedB |> Expect.equal True
                        , \_ -> List.isEmpty mergedR |> Expect.equal True
                        ]
                        ()

                else
                    Expect.all
                        [ \_ ->
                            List.any (\( lk, _ ) -> lk == key) mergedL
                                |> Expect.equal (lMember && not rMember)
                        , \_ ->
                            List.any (\( bk, _, _ ) -> bk == key) mergedB
                                |> Expect.equal (lMember && rMember)
                        , \_ ->
                            List.any (\( rk, _ ) -> rk == key) mergedR
                                |> Expect.equal (not lMember && rMember)
                        ]
                        ()
        ]
