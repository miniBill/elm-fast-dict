module Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, pairListFuzzer, valueFuzzer, veryBalanced, veryUnbalanced)

import FastDict as Dict exposing (Dict)
import Fuzz exposing (Fuzzer)


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
