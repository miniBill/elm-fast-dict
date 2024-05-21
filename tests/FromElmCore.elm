module FromElmCore exposing (suite)

import Common exposing (expectEqual)
import Expect
import FastDict as Dict
import Internal exposing (Dict)
import Test exposing (Test, describe, test)


animals : Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


suite : Test
suite =
    let
        buildTests : Test
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

        queryTests : Test
        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests : Test
        combineTests =
            describe "combine Tests"
                [ test "union" <| \() -> expectEqual animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| \() -> expectEqual (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| \() -> expectEqual (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff" <| \() -> expectEqual (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]

        transformTests : Test
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

        mergeTests : Test
        mergeTests =
            let
                insertBoth : comparable -> appendable -> appendable -> Dict comparable appendable -> Dict comparable appendable
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 : Dict String (List number)
                s1 =
                    Dict.empty |> Dict.insert "u1" [ 1 ]

                s2 : Dict String (List number)
                s2 =
                    Dict.empty |> Dict.insert "u2" [ 2 ]

                s23 : Dict String (List number)
                s23 =
                    Dict.empty |> Dict.insert "u2" [ 3 ]

                b1 : Dict Int (List Int)
                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList

                b2 : Dict Int (List Int)
                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList

                bExpected : List ( number, List number )
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
