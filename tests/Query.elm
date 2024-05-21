module Query exposing (suite)

import Expect
import FastDict as Dict
import Fuzzers exposing (Value, dictFuzzer, keyFuzzer, valueFuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "query"
        [ isEmptyTest
        , memberTest
        , getTest
        , sizeTest
        , equalTest
        ]


isEmptyTest : Test
isEmptyTest =
    describe "isEmpty"
        [ fuzz dictFuzzer "Is true iff the dict is the empty one" <|
            \dict ->
                Dict.isEmpty dict
                    |> Expect.equal (dict == Dict.empty)
        ]


memberTest : Test
memberTest =
    describe "member"
        [ fuzz2 keyFuzzer dictFuzzer "Is true iff get is not Nothing" <|
            \key dict ->
                Dict.member key dict
                    |> Expect.equal (Dict.get key dict /= Nothing)
        , fuzz2 keyFuzzer dictFuzzer "Is equivalent to List.member on the keys" <|
            \key dict ->
                Dict.member key dict
                    |> Expect.equal (List.member key (Dict.keys dict))
        ]


getTest : Test
getTest =
    describe "get"
        [ fuzz2 keyFuzzer valueFuzzer "Retrieves a value from a singleton" <|
            \key value ->
                Dict.get key (Dict.singleton key value)
                    |> Expect.equal (Just value)
        , fuzz keyFuzzer "Retrieves nothing from empty" <|
            \key ->
                Dict.get key Dict.empty
                    |> Expect.equal Nothing
        , fuzz2 keyFuzzer dictFuzzer "Is equivalent to finding in toList" <|
            \key dict ->
                let
                    found : Maybe Value
                    found =
                        Dict.toList dict
                            |> List.filterMap
                                (\( k, v ) ->
                                    if k == key then
                                        Just v

                                    else
                                        Nothing
                                )
                            |> List.head
                in
                Dict.get key dict
                    |> Expect.equal found
        ]


sizeTest : Test
sizeTest =
    describe "size"
        [ fuzz dictFuzzer "Is never negative" <|
            \dict ->
                Dict.size dict
                    |> Expect.greaterThan -1
        , test "Is zero for Dict.empty" <|
            \_ ->
                Dict.size Dict.empty
                    |> Expect.equal 0
        , test "Is one for Dict.singleton" <|
            \_ ->
                Dict.size (Dict.singleton 0 0)
                    |> Expect.equal 1
        , fuzz dictFuzzer "Is zero iff the dictionary is empty" <|
            \dict ->
                (Dict.size dict == 0)
                    |> Expect.equal (Dict.isEmpty dict)
        ]


equalTest : Test
equalTest =
    describe "equal"
        [ test "Different structure means you can't use ==" <|
            \_ ->
                Fuzzers.veryBalanced 12
                    |> Expect.notEqual (Fuzzers.veryUnbalanced 12)
        , fuzz2 dictFuzzer dictFuzzer "Is True iff equivalent via toList" <|
            \left right ->
                (left |> Dict.equals right)
                    |> Expect.equal (Dict.toList left == Dict.toList right)
        , test "Different structure is recognized as equal" <|
            \_ ->
                Fuzzers.veryBalanced 12
                    |> Dict.equals (Fuzzers.veryUnbalanced 12)
                    |> Expect.equal True
        ]
