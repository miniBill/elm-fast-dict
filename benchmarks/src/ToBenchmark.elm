module ToBenchmark exposing (Function, Graph, config)

import Codec exposing (Codec)
import Dict as CoreDict
import FastBenchmark.Types exposing (Config, Param)
import FastDict
import List.Extra
import Random


config : Config Graph Function
config =
    { graphToString = graphToString
    , graphCodec = graphCodec
    , functionToString = functionToString
    , functionCodec = functionCodec

    --
    , graphs = graphs
    , functions = functions
    , sizes = sizes
    , toFunction = toFunction

    --
    , timeout = timeout
    }


type Graph
    = Intersect Ratio Overlap
    | Union Ratio Overlap


type alias Ratio =
    ( Int, Int )


type Overlap
    = OverlapRandom
    | OverlapFull
    | OverlapNoneLeftLower
    | OverlapNoneRightLower
    | OverlapNoneEvenOdd


graphs : List Graph
graphs =
    unionGraphs


intersectGraphs : List Graph
intersectGraphs =
    List.Extra.lift2
        Tuple.pair
        overlaps
        ratios
        |> List.filter (\( overlap, ratio ) -> ratio == ( 1, 1 ) || overlap /= OverlapFull)
        |> List.map (\( overlap, ratio ) -> Intersect ratio overlap)


unionGraphs : List Graph
unionGraphs =
    List.Extra.lift2
        Tuple.pair
        overlaps
        ratios
        |> List.filter (\( overlap, ratio ) -> ratio == ( 1, 1 ) || overlap /= OverlapFull)
        |> List.map (\( overlap, ratio ) -> Union ratio overlap)


ratios : List Ratio
ratios =
    [ ( 1, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( 30, 1 )
    , ( 10, 1 )
    , ( 1, 10 )
    , ( 1, 30 )
    ]


overlaps : List Overlap
overlaps =
    [ OverlapRandom
    , OverlapFull
    , OverlapNoneEvenOdd
    , OverlapNoneLeftLower
    , OverlapNoneRightLower
    ]


graphToString : Graph -> String
graphToString graph =
    case graph of
        Intersect ratio overlap ->
            "intersect " ++ ratioToString ratio ++ " " ++ overlapToString overlap

        Union ratio overlap ->
            "union " ++ ratioToString ratio ++ " " ++ overlapToString overlap


ratioToString : Ratio -> String
ratioToString ( l, r ) =
    String.fromInt l ++ ":" ++ String.fromInt r


overlapToString : Overlap -> String
overlapToString overlap =
    case overlap of
        OverlapFull ->
            "100% shared"

        OverlapRandom ->
            "~50% shared"

        OverlapNoneLeftLower ->
            "0% shared (left < right)"

        OverlapNoneRightLower ->
            "0% shared (left > right)"

        OverlapNoneEvenOdd ->
            "0% shared (left odd, right even)"


graphCodec : Codec Graph
graphCodec =
    Codec.custom
        (\fintersect funion value ->
            case value of
                Intersect ratio overlap ->
                    fintersect ratio overlap

                Union ratio overlap ->
                    funion ratio overlap
        )
        |> Codec.variant2 "Intersect" Intersect (Codec.tuple Codec.int Codec.int) overlapCodec
        |> Codec.variant2 "Union" Union (Codec.tuple Codec.int Codec.int) overlapCodec
        |> Codec.buildCustom


overlapCodec : Codec Overlap
overlapCodec =
    Codec.custom
        (\frandom ffull fnoneLeftLower fnoneRightLower fnoneEvenOdd value ->
            case value of
                OverlapRandom ->
                    frandom

                OverlapFull ->
                    ffull

                OverlapNoneLeftLower ->
                    fnoneLeftLower

                OverlapNoneRightLower ->
                    fnoneRightLower

                OverlapNoneEvenOdd ->
                    fnoneEvenOdd
        )
        |> Codec.variant0 "OverlapRandom" OverlapRandom
        |> Codec.variant0 "OverlapFull" OverlapFull
        |> Codec.variant0 "OverlapNoneLeftLower" OverlapNoneLeftLower
        |> Codec.variant0 "OverlapNoneRightLower" OverlapNoneRightLower
        |> Codec.variant0 "OverlapNoneEvenOdd" OverlapNoneEvenOdd
        |> Codec.buildCustom


type Function
    = Core
    | Fast


functions : List Function
functions =
    [ Core
    , Fast
    ]


functionToString : Function -> String
functionToString function =
    case function of
        Core ->
            "core"

        Fast ->
            "fast"


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\fcore ffast value ->
            case value of
                Core ->
                    fcore

                Fast ->
                    ffast
        )
        |> Codec.variant0 "Core" Core
        |> Codec.variant0 "Fast" Fast
        |> Codec.buildCustom


sizes : List Int
sizes =
    List.range 1 16
        |> List.map (\n -> 2 ^ n)


type alias Both k v =
    { core : CoreDict.Dict k v
    , fast : FastDict.Dict k v
    }


mapBoth : (k -> v -> v) -> Both k v -> Both k v
mapBoth f both =
    { core = CoreDict.map f both.core
    , fast = FastDict.map f both.fast
    }


toFunction : Param Graph Function -> (() -> ())
toFunction { graph, function, size } =
    case graph of
        Intersect ratio overlap ->
            let
                ( ls, rs ) =
                    fromRatioOverlap size ratio overlap
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.intersect ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.intersect ls.fast rs.fast

        Union ratio overlap ->
            let
                ( ls, rs ) =
                    fromRatioOverlap size ratio overlap
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.union ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.union ls.fast rs.fast


fromRatioOverlap : Int -> Ratio -> Overlap -> ( Both Int Int, Both Int Int )
fromRatioOverlap size ratio overlap =
    let
        ( lratio, rratio ) =
            ratio

        lsize : Int
        lsize =
            size * lratio

        rsize : Int
        rsize =
            size * rratio

        rsizeFixed : Int
        rsizeFixed =
            if rsize == lsize then
                -- Prevent having the exact same size, and thus random seed
                rsize + 1

            else
                rsize

        ls : Both Int Int
        ls =
            if overlap == OverlapNoneEvenOdd then
                mapBoth (\_ n -> n * 2) (generate lsize)

            else
                generate lsize

        rs : Both Int Int
        rs =
            generate rsizeFixed

        rsFixed : Both Int Int
        rsFixed =
            if lratio * rratio == 0 then
                -- If we're in the x:0 or 0:x case, just keep it as it is
                rs

            else
                case overlap of
                    OverlapRandom ->
                        rs

                    OverlapFull ->
                        ls

                    OverlapNoneLeftLower ->
                        mapBoth (\_ n -> n + max lsize rsizeFixed * 3) rs

                    OverlapNoneRightLower ->
                        mapBoth (\_ n -> -n) rs

                    OverlapNoneEvenOdd ->
                        mapBoth (\_ n -> n * 2 + 1) rs
    in
    ( ls, rsFixed )


{-| `generate n` generates a list of n numbers between 0 and 2n
-}
generate : Int -> Both Int Int
generate size =
    let
        generator : Random.Generator (Both Int Int)
        generator =
            Random.int 0 (2 * size)
                |> Random.map (\t -> ( t, t ))
                |> Random.list size
                |> Random.map
                    (\lst ->
                        { core = CoreDict.fromList lst
                        , fast = FastDict.fromList lst
                        }
                    )
    in
    Random.step generator (Random.initialSeed size)
        |> Tuple.first


ignore : a -> ()
ignore _ =
    ()


timeout : Maybe Float
timeout =
    Just 3
