module Build exposing (singletonDictTest, suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import FastSet as Set exposing (Set)
import Fuzz exposing (Fuzzer)
import Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, setFuzzer, valueFuzzer)
import Internal exposing (Dict)
import Invariants exposing (expectDictRespectsInvariants)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "build"
        [ describe "dict"
            [ emptyDictTest
            , singletonDictTest
            , insertDictTest
            , updateDictTest
            , removeDictTest
            ]
        , describe "set"
            [ emptySetTest
            , singletonSetTest
            , insertSetTest
            , removeSetTest
            ]
        ]


emptyDictTest : Test
emptyDictTest =
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
        , test "Respects invariants" <|
            \() ->
                expectDictRespectsInvariants Dict.empty
        ]


emptySetTest : Test
emptySetTest =
    describe "empty"
        [ test "Has size 0" <|
            \_ ->
                Set.empty
                    |> Set.size
                    |> Expect.equal 0
        , fuzz keyFuzzer "Does not contain any element" <|
            \key ->
                Set.member key Set.empty
                    |> Expect.equal False
        , test "Has an empty toList" <|
            \_ ->
                Set.empty
                    |> Set.toList
                    |> Expect.equalLists []
        ]


singletonDictTest : Test
singletonDictTest =
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
        , test "Respects invariants" <|
            \() ->
                expectDictRespectsInvariants (Dict.singleton key value)
        ]


singletonSetTest : Test
singletonSetTest =
    let
        key : Key
        key =
            "0"

        singleton : Set Key
        singleton =
            Set.singleton key
    in
    describe "singleton"
        [ test "Has size 1" <|
            \_ ->
                singleton
                    |> Set.size
                    |> Expect.equal 1
        , fuzz keyFuzzer "Only contains its key" <|
            \k ->
                Set.member k singleton
                    |> Expect.equal (k == key)
        , test "Has a singleton toList" <|
            \_ ->
                singleton
                    |> Set.toList
                    |> Expect.equalLists (List.singleton key)
        ]


insertDictTest : Test
insertDictTest =
    let
        insertFuzzer : Fuzzer ( Key, Value, Dict Key Value )
        insertFuzzer =
            Fuzz.triple keyFuzzer valueFuzzer dictFuzzer
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
        , fuzz3 keyFuzzer valueFuzzer dictFuzzer "Respects the invariants" <|
            \key value dict ->
                Dict.insert key value dict
                    |> expectDictRespectsInvariants
        ]


insertSetTest : Test
insertSetTest =
    let
        insertFuzzer : Fuzzer ( Key, Set Key )
        insertFuzzer =
            Fuzz.pair keyFuzzer setFuzzer
    in
    describe "insert"
        [ fuzz insertFuzzer "Allows using member to check the value was added" <|
            \( key, dict ) ->
                Set.member key (Set.insert key dict)
                    |> Expect.equal True
        , fuzz insertFuzzer "Increases size by 0 (resp. 1) if already a member (resp. if not)" <|
            \( key, set ) ->
                let
                    increment : Int
                    increment =
                        if Set.member key set then
                            0

                        else
                            1
                in
                Set.size (Set.insert key set)
                    |> Expect.equal (Set.size set + increment)
        , fuzz insertFuzzer "Is idempotent" <|
            \( key, set ) ->
                let
                    intermediate : Set Key
                    intermediate =
                        set
                            |> Set.insert key
                in
                intermediate
                    |> Set.insert key
                    |> Set.equals intermediate
                    |> Expect.equal True
        ]


updateDictTest : Test
updateDictTest =
    let
        updateFuzzer : Fuzzer ( Key, Dict Key Value )
        updateFuzzer =
            Fuzz.pair keyFuzzer dictFuzzer

        functionFuzzer : Fuzzer (Maybe number -> Maybe number)
        functionFuzzer =
            Fuzz.oneOf
                [ Fuzz.constant (\_ -> Nothing)
                , Fuzz.constant (\_ -> Just 1)
                , Fuzz.constant identity
                ]
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
        , fuzz3 keyFuzzer functionFuzzer dictFuzzer "Respects the invariants" <|
            \key value dict ->
                Dict.update key value dict
                    |> expectDictRespectsInvariants
        ]


removeDictTest : Test
removeDictTest =
    let
        removeFuzzer : Fuzzer ( Key, Dict Key Value )
        removeFuzzer =
            Fuzz.pair keyFuzzer dictFuzzer
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
        , fuzz removeFuzzer "Respects the invariants" <|
            \( key, dict ) ->
                Dict.remove key dict
                    |> expectDictRespectsInvariants
        ]


removeSetTest : Test
removeSetTest =
    let
        removeFuzzer : Fuzzer ( Key, Set Key )
        removeFuzzer =
            Fuzz.pair keyFuzzer setFuzzer
    in
    describe "remove"
        [ fuzz removeFuzzer "Will make sure a key is not present after deletion" <|
            \( key, set ) ->
                Set.member key (Set.remove key set)
                    |> Expect.equal False
        , fuzz removeFuzzer "Decreases size by 1 (resp. 0) if a member (resp. if not)" <|
            \( key, set ) ->
                let
                    decrement : Int
                    decrement =
                        if Set.member key set then
                            1

                        else
                            0
                in
                Set.size (Set.remove key set)
                    |> Expect.equal (Set.size set - decrement)
        , fuzz removeFuzzer "Doesn't touch the set if the key is not present" <|
            \( key, set ) ->
                (Set.remove key set == set)
                    |> Expect.equal (not (Set.member key set))
        ]
