module FastDictTest exposing (suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import Fuzz exposing (Fuzzer)
import Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, pairListFuzzer)
import Internal exposing (Dict(..), InnerDict(..), NColor(..))
import Invariants exposing (respectsInvariantsFuzz)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "FastDict"
        [ -- Min / max
          getMinKeyTest
        , getMinTest
        , getMaxKeyTest
        , getMaxTest
        , popMinTest
        , popMaxTest

        -- Lists
        , keysTest
        , valuesTest
        , toListTest
        , fromListTest

        -- Transform
        , mapTest
        , foldlTest
        , foldrTest
        , filterTest
        , partitionTest

        -- Combine
        , unionTest
        , intersectTest
        , diffTest
        , mergeTest

        -- elm/core
        , elmCoreTests
        ]



-- Min / max --


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



-- Lists --


keysTest : Test
keysTest =
    describe "keys"
        [ fuzz dictFuzzer "Is equivalent to List.map Tuple.first toList" <|
            \dict ->
                dict
                    |> Dict.keys
                    |> Expect.equalLists (List.map Tuple.first (Dict.toList dict))
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.keys dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        , fuzz dictFuzzer "Is sorted" <|
            \dict ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (List.sort keys)
        , fuzz dictFuzzer "Contains no duplicates" <|
            \dict ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (dedupBy identity <| List.sort keys)
        ]


valuesTest : Test
valuesTest =
    describe "values"
        [ fuzz dictFuzzer "Is equivalent to List.map Tuple.second toList" <|
            \dict ->
                dict
                    |> Dict.values
                    |> Expect.equalLists (List.map Tuple.second (Dict.toList dict))
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.values dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


toListTest : Test
toListTest =
    describe "toList"
        [ fuzz dictFuzzer "Is sorted by key" <|
            \dict ->
                dict
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictFuzzer "Has the correct size" <|
            \dict ->
                Dict.toList dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


fromListTest : Test
fromListTest =
    describe "fromList"
        [ fuzz pairListFuzzer "Combined with toList is the equivalent of sort >> dedupBy Tuple.first" <|
            \list ->
                list
                    |> Dict.fromList
                    |> Dict.toList
                    |> Expect.equalLists (dedupBy Tuple.first (List.sortBy Tuple.first list))
        , fuzz dictFuzzer "Is the inverse to toList" <|
            \dict ->
                dict
                    |> Dict.toList
                    |> Dict.fromList
                    |> Dict.toList
                    |> Expect.equal (Dict.toList dict)
        , respectsInvariantsFuzz dictFuzzer
        ]



-- Transform --


mapTest : Test
mapTest =
    let
        f1 k v =
            k ++ " " ++ String.fromInt v

        f2 _ v =
            String.fromInt <| v + 1

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



-- Combine --


unionTest : Test
unionTest =
    let
        unionFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

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
        intersectFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

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
        diffFuzzer =
            Fuzz.pair dictFuzzer dictFuzzer

        diffedFuzzer =
            Fuzz.map2 Dict.diff dictFuzzer dictFuzzer
    in
    describe "diff"
        [ fuzz2 diffFuzzer keyFuzzer "Contains the correct values giving preference to the first" <|
            \( first, second ) key ->
                let
                    diff =
                        Dict.diff first second

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



-- Utils --


dedupBy : (a -> b) -> List a -> List a
dedupBy f =
    List.foldr
        (\e acc ->
            case acc of
                [] ->
                    [ e ]

                last :: _ ->
                    if f e == f last then
                        acc

                    else
                        e :: acc
        )
        []


animals : Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


elmCoreTests : Test
elmCoreTests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <| \() -> expectEqual (Dict.fromList []) Dict.empty
                , test "singleton" <| \() -> expectEqual (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <| \() -> expectEqual (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
                , test "insert replace" <| \() -> expectEqual (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <| \() -> expectEqual (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\_ -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <| \() -> expectEqual Dict.empty (Dict.update "k" (\_ -> Nothing) (Dict.singleton "k" "v"))
                , test "remove" <| \() -> expectEqual Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
                , test "remove not found" <| \() -> expectEqual (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
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
                [ test "union" <| \() -> expectEqual animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| \() -> expectEqual (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| \() -> expectEqual (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff" <| \() -> expectEqual (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> expectEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k _ -> k == "Tom") animals)
                , test "partition" <|
                    \() ->
                        Expect.all
                            (let
                                ( pl, pr ) =
                                    Dict.partition (\k _ -> k == "Tom") animals
                             in
                             [ \_ -> expectEqual (Dict.singleton "Tom" "cat") pl
                             , \_ -> expectEqual (Dict.singleton "Jerry" "mouse") pr
                             ]
                            )
                            ()
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
                        expectEqual Dict.empty
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
