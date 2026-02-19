module Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, pairListFuzzer, setFuzzer, valueFuzzer, veryBalancedDict, veryUnbalancedDict)

import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import Fuzz exposing (Fuzzer)


type alias Key =
    String


type alias Value =
    Int


dictFuzzer : Fuzzer (Dict Key Value)
dictFuzzer =
    Fuzz.oneOf
        [ fromListDictFuzzer
        , fromOpsDictFuzzer
        , Fuzz.map veryBalancedDict (Fuzz.intRange 0 1024)
        , Fuzz.map veryUnbalancedDict (Fuzz.intRange 0 1024)
        ]


setFuzzer : Fuzzer (Set Key)
setFuzzer =
    Fuzz.oneOf
        [ fromListSetFuzzer
        , fromOpsSetFuzzer
        , Fuzz.map veryBalancedSet (Fuzz.intRange 0 1024)
        , Fuzz.map veryUnbalancedSet (Fuzz.intRange 0 1024)
        ]


fromOpsDictFuzzer : Fuzzer (Dict Key Value)
fromOpsDictFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
        |> Fuzz.map (List.foldl applyDictOp Dict.empty)


applyDictOp : Op -> Dict Key Value -> Dict Key Value
applyDictOp op acc =
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


fromOpsSetFuzzer : Fuzzer (Set Key)
fromOpsSetFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
        |> Fuzz.map (List.foldl applySetOp Set.empty)


applySetOp : Op -> Set Key -> Set Key
applySetOp op acc =
    case op of
        Insert k _ ->
            Set.insert k acc

        Delete index ->
            let
                listed : List Key
                listed =
                    Set.toList acc

                fixedIndex : Int
                fixedIndex =
                    -- the *2 makes it a 50% chance of deleting
                    -- the +1 avoids a division by zero
                    modBy (List.length listed * 2 + 1) index
            in
            case List.drop fixedIndex (Set.toList acc) of
                key :: _ ->
                    Set.remove key acc

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


fromListDictFuzzer : Fuzzer (Dict Key Value)
fromListDictFuzzer =
    pairListFuzzer
        |> Fuzz.map Dict.fromList


pairListFuzzer : Fuzzer (List ( Key, Value ))
pairListFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.listOfLengthBetween 1 100


fromListSetFuzzer : Fuzzer (Set Key)
fromListSetFuzzer =
    keyFuzzer
        |> Fuzz.listOfLengthBetween 1 100
        |> Fuzz.map Set.fromList


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


veryBalancedDict : Int -> Dict Key Value
veryBalancedDict n =
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
                    mid : Int
                    mid =
                        low + (high - low) // 2
                in
                acc
                    |> insert mid
                    |> go low (mid - 1)
                    |> go (mid + 1) high
    in
    go 1 n Dict.empty


veryUnbalancedDict : Int -> Dict Key Value
veryUnbalancedDict n =
    List.range 1 n
        |> List.map (\k -> ( String.fromInt k, k ))
        |> Dict.fromList


veryBalancedSet : Int -> Set Key
veryBalancedSet n =
    let
        insert : Int -> Set Key -> Set Key
        insert k =
            Set.insert (String.fromInt k)

        go : Int -> Int -> Set Key -> Set Key
        go low high acc =
            if low >= high then
                insert low acc

            else
                let
                    mid : Int
                    mid =
                        low + (high - low) // 2
                in
                acc
                    |> insert mid
                    |> go low (mid - 1)
                    |> go (mid + 1) high
    in
    go 1 n Set.empty


veryUnbalancedSet : Int -> Set Key
veryUnbalancedSet n =
    List.range 1 n
        |> List.map String.fromInt
        |> Set.fromList
