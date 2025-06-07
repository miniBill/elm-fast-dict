module Build exposing (singletonTest, suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import Fuzz exposing (Fuzzer)
import Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, valueFuzzer)
import Internal exposing (Dict)
import Invariants exposing (respectsInvariants, respectsInvariantsFuzz)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "build"
        [ emptyTest
        , singletonTest
        , insertTest
        , updateTest
        , removeTest
        ]


emptyTest : Test
emptyTest =
    describe "empty"
        [ test "Has size 0" <|
            \_ ->
                Dict.empty
                    |> Dict.size
                    |> Expect.equal 0
        , fuzz keyFuzzer "Does not contain any element" <|
            \key ->
                Dict.member key Dict.empty
                    |> Expect.equal False
        , test "Has an empty toList" <|
            \_ ->
                Dict.empty
                    |> Dict.toList
                    |> Expect.equalLists []
        , respectsInvariants Dict.empty
        ]


singletonTest : Test
singletonTest =
    let
        key : Key
        key =
            "0"

        value : Value
        value =
            1

        singleton : Dict Key Value
        singleton =
            Dict.singleton key value
    in
    describe "singleton"
        [ test "Has size 1" <|
            \_ ->
                singleton
                    |> Dict.size
                    |> Expect.equal 1
        , fuzz keyFuzzer "Only contains its key" <|
            \k ->
                Dict.member k singleton
                    |> Expect.equal (k == key)
        , test "Has a singleton toList" <|
            \_ ->
                singleton
                    |> Dict.toList
                    |> Expect.equalLists (List.singleton ( key, value ))
        , respectsInvariants (Dict.singleton key value)
        ]


insertTest : Test
insertTest =
    let
        insertFuzzer : Fuzzer ( Key, Value, Dict Key Value )
        insertFuzzer =
            Fuzz.triple keyFuzzer valueFuzzer dictFuzzer

        insertedFuzzer : Fuzzer (Dict Key Value)
        insertedFuzzer =
            Fuzz.map3 Dict.insert keyFuzzer valueFuzzer dictFuzzer
    in
    describe "insert"
        [ fuzz insertFuzzer "Allows using get to return the same value" <|
            \( key, value, dict ) ->
                Dict.get key (Dict.insert key value dict)
                    |> Expect.equal (Just value)
        , fuzz insertFuzzer "Increases size by 0 (resp. 1) if already a member (resp. if not)" <|
            \( key, value, dict ) ->
                let
                    increment : Int
                    increment =
                        if Dict.member key dict then
                            0

                        else
                            1
                in
                Dict.size (Dict.insert key value dict)
                    |> Expect.equal (Dict.size dict + increment)
        , fuzz2 insertFuzzer valueFuzzer "Overwrites existing values" <|
            \( key, value, dict ) value2 ->
                dict
                    |> Dict.insert key value
                    |> Dict.insert key value2
                    |> Dict.get key
                    |> Expect.equal (Just value2)
        , respectsInvariantsFuzz identity insertedFuzzer
        ]


updateTest : Test
updateTest =
    let
        updateFuzzer : Fuzzer ( Key, Dict Key Value )
        updateFuzzer =
            Fuzz.pair keyFuzzer dictFuzzer

        updatedFuzzer : Fuzzer (Dict Key Value)
        updatedFuzzer =
            Fuzz.map3 Dict.update
                keyFuzzer
                (Fuzz.oneOf
                    [ Fuzz.constant (\_ -> Nothing)
                    , Fuzz.constant (\_ -> Just 1)
                    , Fuzz.constant identity
                    ]
                )
                dictFuzzer
    in
    describe "update"
        {- These tests use `Expect.equal` which would normally be too strict,
           but since in the future `update` could be rewritten by melding get/insert/delete
           we want to make sure that the structure is correctly preserved.
        -}
        [ fuzz updateFuzzer "update k (\\_ -> Nothing) is equivalent to remove k" <|
            \( key, dict ) ->
                dict
                    |> Dict.update key (\_ -> Nothing)
                    |> expectEqual (Dict.remove key dict)
        , fuzz2 updateFuzzer valueFuzzer "update k (\\_ -> Just v) is equivalent to insert k v" <|
            \( key, dict ) value ->
                dict
                    |> Dict.update key (\_ -> Just value)
                    |> expectEqual (Dict.insert key value dict)
        , fuzz updateFuzzer "update k identity is equivalent to identity" <|
            \( key, dict ) ->
                dict
                    |> Dict.update key identity
                    |> expectEqual dict
        , respectsInvariantsFuzz identity updatedFuzzer
        ]


removeTest : Test
removeTest =
    let
        removeFuzzer : Fuzzer ( Key, Dict Key Value )
        removeFuzzer =
            Fuzz.pair keyFuzzer dictFuzzer

        removedFuzzer : Fuzzer (Dict Key Value)
        removedFuzzer =
            Fuzz.map2 Dict.remove keyFuzzer dictFuzzer
    in
    describe "remove"
        [ fuzz removeFuzzer "Will make sure a key is not present after deletion" <|
            \( key, dict ) ->
                Dict.get key (Dict.remove key dict)
                    |> Expect.equal Nothing
        , fuzz removeFuzzer "Decreases size by 1 (resp. 0) if a member (resp. if not)" <|
            \( key, dict ) ->
                let
                    decrement : Int
                    decrement =
                        if Dict.member key dict then
                            1

                        else
                            0
                in
                Dict.size (Dict.remove key dict)
                    |> Expect.equal (Dict.size dict - decrement)
        , fuzz removeFuzzer "Doesn't touch the dictionary if the key is not present" <|
            \( key, dict ) ->
                (Dict.remove key dict == dict)
                    |> Expect.equal (not (Dict.member key dict))
        , respectsInvariantsFuzz identity removedFuzzer
        ]
