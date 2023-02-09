module FastDictTest exposing (suite)

-- import Dict as CoreDict
--exposing (Expectation)

import Expect
import FastDict as Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "FastDict"
        [ -- Build
          emptyTest
        , singletonTest
        , insertTest
        , updateTest
        , removeTest

        -- Query
        , isEmptyTest
        , sizeTest
        , memberTest

        -- elm/core
        , elmCoreTests
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
        ]


insertTest : Test
insertTest =
    let
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
        ]


updateTest : Test
updateTest =
    describe "update"
        {- These tests use `Expect.equal` which would normally be too strict,
           but since in the future `update` could be rewritten by melding get/insert/delete
           we want to make sure that the structure is correctly preserved.
        -}
        [ fuzz2 keyFuzzer dictFuzzer "update k (\\_ -> Nothing) is equivalent to remove k" <|
            \key dict ->
                dict
                    |> Dict.update key (\_ -> Nothing)
                    |> Expect.equal (Dict.remove key dict)
        , fuzz3 keyFuzzer valueFuzzer dictFuzzer "update k (\\_ -> Just v) is equivalent to insert k v" <|
            \key value dict ->
                dict
                    |> Dict.update key (\_ -> Just value)
                    |> Expect.equal (Dict.insert key value dict)
        , fuzz2 keyFuzzer dictFuzzer "update k identity is equivalent to identity" <|
            \key dict ->
                dict
                    |> Dict.update key identity
                    |> Expect.equal dict
        ]


removeTest : Test
removeTest =
    let
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



{-
   expectEqual : Dict comparable v -> Dict comparable v -> Expectation
   expectEqual expected actual =
       actual
           |> Dict.toList
           |> CoreDict.fromList
           |> Expect.equalDicts (CoreDict.fromList <| Dict.toList expected)
-}


sizeTest : Test
sizeTest =
    describe "size"
        [ fuzz dictFuzzer "Is never negative" <|
            \d ->
                Dict.size d
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
            \d ->
                (Dict.size d == 0)
                    |> Expect.equal (Dict.isEmpty d)
        ]


type alias Key =
    String


type alias Value =
    Int


dictFuzzer : Fuzzer (Dict Key Value)
dictFuzzer =
    Fuzz.oneOf
        [ fromListFuzzer
        , fromOpsFuzzer
        ]


fromOpsFuzzer : Fuzzer (Dict Key Value)
fromOpsFuzzer =
    opFuzzer
        |> Fuzz.list
        |> Fuzz.map (List.foldl applyOp Dict.empty)


applyOp : Op -> Dict Key Value -> Dict Key Value
applyOp op acc =
    case op of
        Insert k v ->
            Dict.insert k v acc

        Delete index ->
            let
                listed : List ( Key, Value )
                listed =
                    Dict.toList acc

                fixedIndex : Int
                fixedIndex =
                    -- the *2 makes it a 50% chance of deleting
                    -- the +1 avoids a division by zero
                    modBy (List.length listed * 2 + 1) index
            in
            case List.drop fixedIndex (Dict.keys acc) of
                key :: _ ->
                    Dict.remove key acc

                _ ->
                    acc


opFuzzer : Fuzzer Op
opFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Insert keyFuzzer valueFuzzer
        , Fuzz.map Delete Fuzz.int
        ]


type Op
    = Insert Key Value
    | Delete Int


fromListFuzzer : Fuzzer (Dict Key Value)
fromListFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.list
        |> Fuzz.map Dict.fromList


keyFuzzer : Fuzzer Key
keyFuzzer =
    Fuzz.oneOf
        [ Fuzz.intRange 0 10 -- provoke more collisions
        , Fuzz.int
        ]
        |> Fuzz.map String.fromInt


valueFuzzer : Fuzzer Int
valueFuzzer =
    Fuzz.int


animals : Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


elmCoreTests : Test
elmCoreTests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <| \() -> Expect.equal (Dict.fromList []) Dict.empty
                , test "singleton" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
                , test "insert replace" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\_ -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <| \() -> Expect.equal Dict.empty (Dict.update "k" (\_ -> Nothing) (Dict.singleton "k" "v"))
                , test "remove" <| \() -> Expect.equal Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
                , test "remove not found" <| \() -> Expect.equal (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <| \() -> Expect.equal animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff" <| \() -> Expect.equal (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter (\k _ -> k == "Tom") animals)
                , test "partition" <| \() -> Expect.equal ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" ) (Dict.partition (\k _ -> k == "Tom") animals)
                ]

        mergeTests =
            let
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 =
                    Dict.empty |> Dict.insert "u1" [ 1 ]

                s2 =
                    Dict.empty |> Dict.insert "u2" [ 2 ]

                s23 =
                    Dict.empty |> Dict.insert "u2" [ 3 ]

                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList

                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList

                bExpected =
                    [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
            in
            describe "merge Tests"
                [ test "merge empties" <|
                    \() ->
                        Expect.equal Dict.empty
                            (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)
                , test "merge singletons in order" <|
                    \() ->
                        Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                            (Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty |> Dict.toList)
                , test "merge singletons out of order" <|
                    \() ->
                        Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                            (Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty |> Dict.toList)
                , test "merge with duplicate key" <|
                    \() ->
                        Expect.equal [ ( "u2", [ 2, 3 ] ) ]
                            (Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty |> Dict.toList)
                , test "partially overlapping" <|
                    \() ->
                        Expect.equal bExpected
                            (Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty |> Dict.toList)
                ]
    in
    describe "Dict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , mergeTests
        ]
