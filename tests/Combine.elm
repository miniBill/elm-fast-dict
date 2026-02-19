module Combine exposing (suite)

import Expect
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import Fuzz
import Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, setFuzzer)
import Invariants exposing (expectDictRespectsInvariants)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


suite : Test
suite =
    describe "combine"
        [ describe "dict"
            [ unionDictTest
            , intersectDictTest
            , diffDictTest
            , mergeDictTest
            ]
        , describe "set"
            [ unionSetTest
            , intersectSetTest
            , diffSetTest
            ]
        ]


unionDictTest : Test
unionDictTest =
    let
        unionFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        unionFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer
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
        , fuzz unionFuzzer "Respects the invariants" <|
            \( first, second ) ->
                Dict.union first second
                    |> expectDictRespectsInvariants
        ]


unionSetTest : Test
unionSetTest =
    let
        unionFuzzer : Fuzz.Fuzzer ( Set Key, Set Key )
        unionFuzzer =
            Fuzz.pair setFuzzer setFuzzer
    in
    describe "union"
        [ fuzz2 unionFuzzer keyFuzzer "Contains the correct keys" <|
            \( first, second ) key ->
                case
                    ( Set.member key first
                    , Set.member key second
                    , Set.member key (Set.union first second)
                    )
                of
                    ( True, _, True ) ->
                        Expect.pass

                    ( _, True, True ) ->
                        Expect.pass

                    ( False, False, False ) ->
                        Expect.pass

                    ( True, _, False ) ->
                        Expect.fail "Value found in first but not in union"

                    ( _, True, False ) ->
                        Expect.fail "Value found in second but not in union"

                    ( False, False, True ) ->
                        Expect.fail "Value found in union but not in first nor second"
        ]


intersectDictTest : Test
intersectDictTest =
    let
        intersectFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        intersectFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer
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
        , fuzz intersectFuzzer "Respects the invariants" <|
            \( first, second ) ->
                Dict.intersect first second
                    |> expectDictRespectsInvariants
        ]


intersectSetTest : Test
intersectSetTest =
    let
        intersectFuzzer : Fuzz.Fuzzer ( Set Key, Set Key )
        intersectFuzzer =
            Fuzz.pair setFuzzer setFuzzer
    in
    describe "intersect"
        [ fuzz2 intersectFuzzer keyFuzzer "Contains the correct keys" <|
            \( first, second ) key ->
                case
                    ( Set.member key first
                    , Set.member key second
                    , Set.member key (Set.intersect first second)
                    )
                of
                    ( True, True, True ) ->
                        Expect.pass

                    ( False, _, False ) ->
                        Expect.pass

                    ( _, False, False ) ->
                        Expect.pass

                    ( False, _, True ) ->
                        Expect.fail "Value found in intersection but not in first"

                    ( _, False, True ) ->
                        Expect.fail "Value found in intersection but not in second"

                    ( True, True, False ) ->
                        Expect.fail "Value found in both but not in intersection"
        ]


diffDictTest : Test
diffDictTest =
    let
        diffFuzzer : Fuzz.Fuzzer ( Dict Key Value, Dict Key Value )
        diffFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer
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
        , fuzz diffFuzzer "Respects the invariants" <|
            \( first, second ) ->
                Dict.diff first second
                    |> expectDictRespectsInvariants
        ]


diffSetTest : Test
diffSetTest =
    let
        diffFuzzer : Fuzz.Fuzzer ( Set Key, Set Key )
        diffFuzzer =
            Fuzz.pair setFuzzer setFuzzer
    in
    describe "diff"
        [ fuzz2 diffFuzzer keyFuzzer "Contains the correct keys" <|
            \( first, second ) key ->
                case
                    ( Set.member key first
                    , Set.member key second
                    , Set.member key (Set.diff first second)
                    )
                of
                    ( _, True, False ) ->
                        Expect.pass

                    ( True, False, True ) ->
                        Expect.pass

                    ( False, _, False ) ->
                        Expect.pass

                    ( True, True, True ) ->
                        Expect.fail "Value found in both and in difference"

                    ( True, False, False ) ->
                        Expect.fail "Value found in first and not in second but not found in intersection"

                    ( False, _, True ) ->
                        Expect.fail "Value not found in first but found in intersection"
        ]


mergeDictTest : Test
mergeDictTest =
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
                in
                if Dict.isEmpty left && Dict.isEmpty right then
                    Expect.all
                        [ \_ -> List.isEmpty mergedL |> Expect.equal True
                        , \_ -> List.isEmpty mergedB |> Expect.equal True
                        , \_ -> List.isEmpty mergedR |> Expect.equal True
                        ]
                        ()

                else
                    let
                        ( lMember, rMember ) =
                            ( Dict.member key left, Dict.member key right )
                    in
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
