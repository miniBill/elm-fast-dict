module FastDictTest exposing (suite)

import Dict as CoreDict
import Expect exposing (Expectation)
import FastDict as Dict
import Fuzz exposing (Fuzzer)
import Internal exposing (Dict(..), InnerDict(..), NColor(..))
import Invariants
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
        , memberTest
        , getTest
        , sizeTest
        , equalTest

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



-- Build --


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
        , respectsInvariantsFuzz insertedFuzzer
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
        , respectsInvariantsFuzz updatedFuzzer
        ]


removeTest : Test
removeTest =
    let
        removeFuzzer =
            Fuzz.pair keyFuzzer dictFuzzer

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
        , respectsInvariantsFuzz removedFuzzer
        ]



-- Query --


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
    describe "equal is not magic"
        [ test "Different structure means you can't use ==" <|
            \_ ->
                veryBalanced 12
                    |> Expect.notEqual (veryUnbalanced 12)
        , fuzz2 dictFuzzer dictFuzzer "Is True iff equivalent via toList" <|
            \left right ->
                (left |> Dict.equals right)
                    |> Expect.equal (Dict.toList left == Dict.toList right)
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


respectsInvariants : Dict Key Value -> Test
respectsInvariants dict =
    describe "Respects the invariants"
        [ test "The root is black" <|
            \_ ->
                dict
                    |> Invariants.isRootBlack
                    |> Expect.equal True
        , test "The cached size is correct" <|
            \_ ->
                dict
                    |> Invariants.hasCorrectSize
                    |> Expect.equal True
        , test "It is a BST" <|
            \_ ->
                dict
                    |> Invariants.isBst
                    |> Expect.equal True
        , test "The black height is consistent" <|
            \_ ->
                dict
                    |> Invariants.blackHeight
                    |> Expect.notEqual Nothing
        , test "No red node has a red child" <|
            \_ ->
                dict
                    |> Invariants.noRedChildOfRedNode
                    |> Expect.equal True
        ]


{-| Checks whether a dictionary respects the four invariants:

1.  the root is black
2.  the cached size is the amount of inner nodes
3.  the tree is a BST
4.  the black height is equal on all branches

-}
respectsInvariantsFuzz : Fuzzer (Dict Key value) -> Test
respectsInvariantsFuzz fuzzer =
    describe "Respects the invariants"
        [ fuzz fuzzer "The root is black" <|
            \dict ->
                dict
                    |> Invariants.isRootBlack
                    |> Expect.equal True
        , fuzz fuzzer "The cached size is correct" <|
            \dict ->
                dict
                    |> Invariants.hasCorrectSize
                    |> Expect.equal True
        , fuzz fuzzer "It is a BST" <|
            \dict ->
                dict
                    |> Invariants.isBst
                    |> Expect.equal True
        , fuzz fuzzer "The black height is consistent" <|
            \dict ->
                dict
                    |> Invariants.blackHeight
                    |> Expect.notEqual Nothing
        , fuzz fuzzer "No red node has a red child" <|
            \dict ->
                dict
                    |> Invariants.noRedChildOfRedNode
                    |> Expect.equal True
        ]


veryBalanced : Int -> Dict Key Value
veryBalanced n =
    let
        insert : Int -> Dict Key Value -> Dict Key Value
        insert k =
            Dict.insert (String.fromInt k) k

        go : Int -> Int -> Dict Key Value -> Dict Key Value
        go low high acc =
            if low >= high then
                insert low acc

            else
                let
                    mid =
                        low + (high - low) // 2
                in
                acc
                    |> insert mid
                    |> go low (mid - 1)
                    |> go (mid + 1) high
    in
    go 1 n Dict.empty


veryUnbalanced : Int -> Dict Key Value
veryUnbalanced n =
    List.range 1 n
        |> List.map (\k -> ( String.fromInt k, k ))
        |> Dict.fromList


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


expectEqual : Dict comparable v -> Dict comparable v -> Expectation
expectEqual expected actual =
    actual
        |> Dict.toList
        |> CoreDict.fromList
        |> Expect.equalDicts (CoreDict.fromList <| Dict.toList expected)


type alias Key =
    String


type alias Value =
    Int


dictFuzzer : Fuzzer (Dict Key Value)
dictFuzzer =
    Fuzz.oneOf
        [ fromListFuzzer
        , fromOpsFuzzer
        , Fuzz.map veryBalanced (Fuzz.intRange 0 1024)
        , Fuzz.map veryUnbalanced (Fuzz.intRange 0 1024)
        ]


fromOpsFuzzer : Fuzzer (Dict Key Value)
fromOpsFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
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
    pairListFuzzer
        |> Fuzz.map Dict.fromList


pairListFuzzer : Fuzzer (List ( Key, Value ))
pairListFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.listOfLengthBetween 1 100


keyFuzzer : Fuzzer Key
keyFuzzer =
    Fuzz.oneOf
        [ Fuzz.intRange 0 10 -- provoke more collisions
        , Fuzz.int
        ]
        |> Fuzz.map String.fromInt


valueFuzzer : Fuzzer Value
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
