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
    | UnionIsFast Column
    | IntersectIsFast Column
    | EqualsIsFast Column


type Column
    = Comparable
    | Better
    | Best


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
    List.Extra.lift2 identity
        [ UnionIsFast, IntersectIsFast, EqualsIsFast ]
        [ Comparable, Better, Best ]


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

        UnionIsFast Comparable ->
            "union #1 - identical dictionaries"

        UnionIsFast Better ->
            "union #2 - second dictionary is 2x bigger than first one"

        UnionIsFast Best ->
            "union #3 - second dictionary is 100x bigger than first one"

        IntersectIsFast Comparable ->
            "intersect #1 - identical dictionaries"

        IntersectIsFast Better ->
            "intersect #2 - first dictionary is even numbers, second dictionary is odd ones"

        IntersectIsFast Best ->
            "intersect #3 - second dictionary is 10x bigger than first one"

        EqualsIsFast Comparable ->
            "equals #1 - identical dictionaries"

        EqualsIsFast Better ->
            "equals #2 - one element difference"

        EqualsIsFast Best ->
            "equals #3 - different size"


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
        (\fintersect funion fUnionFast fIntersectFast fEqualsFast value ->
            case value of
                Intersect ratio overlap ->
                    fintersect ratio overlap

                Union ratio overlap ->
                    funion ratio overlap

                UnionIsFast col ->
                    fUnionFast col

                IntersectIsFast col ->
                    fIntersectFast col

                EqualsIsFast col ->
                    fEqualsFast col
        )
        |> Codec.variant2 "Intersect" Intersect (Codec.tuple Codec.int Codec.int) overlapCodec
        |> Codec.variant2 "Union" Union (Codec.tuple Codec.int Codec.int) overlapCodec
        |> Codec.variant1 "UnionIsFast" UnionIsFast columnCodec
        |> Codec.variant1 "IntersectIsFast" IntersectIsFast columnCodec
        |> Codec.variant1 "EqualsIsFast" EqualsIsFast columnCodec
        |> Codec.buildCustom


columnCodec : Codec Column
columnCodec =
    Codec.custom
        (\fcomparable fbetter fbest value ->
            case value of
                Comparable ->
                    fcomparable

                Better ->
                    fbetter

                Best ->
                    fbest
        )
        |> Codec.variant0 "Comparable" Comparable
        |> Codec.variant0 "Better" Better
        |> Codec.variant0 "Best" Best
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

        UnionIsFast Comparable ->
            -- union #1 - identical dictionaries
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 1, 1 ) OverlapFull
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.union ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.union ls.fast rs.fast

        UnionIsFast Better ->
            -- union #2 - first dictionary is 2x bigger than second one
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 2, 1 ) OverlapRandom
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.union ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.union ls.fast rs.fast

        UnionIsFast Best ->
            -- union #3 - first dictionary is 100x bigger than second one
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 100, 1 ) OverlapRandom
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.union ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.union ls.fast rs.fast

        IntersectIsFast Comparable ->
            -- intersect #1 - identical dictionaries
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 1, 1 ) OverlapFull
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.intersect ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.intersect ls.fast rs.fast

        IntersectIsFast Better ->
            -- intersect #2 - first dictionary is even numbers, second dictionary is odd ones
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 1, 1 ) OverlapNoneEvenOdd
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.intersect ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.intersect ls.fast rs.fast

        IntersectIsFast Best ->
            -- intersect #3 - first dictionary is 10x bigger than second one
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 10, 1 ) OverlapRandom
            in
            case function of
                Core ->
                    \_ -> ignore <| CoreDict.intersect ls.core rs.core

                Fast ->
                    \_ -> ignore <| FastDict.intersect ls.fast rs.fast

        EqualsIsFast Comparable ->
            -- equals #1 - identical dictionaries
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 1, 1 ) OverlapFull
            in
            case function of
                Core ->
                    \_ -> ignore <| ls.core == rs.core

                Fast ->
                    \_ -> ignore <| FastDict.equals ls.fast rs.fast

        EqualsIsFast Better ->
            -- equals #2 - one element difference
            let
                ( base, _ ) =
                    fromRatioOverlap size ( 1, 1 ) OverlapRandom

                -- `size / 2` should be in the middle as a key
                ls : Both Int Int
                ls =
                    { core = CoreDict.insert (size // 2) 0 base.core
                    , fast = FastDict.insert (size // 2) 0 base.fast
                    }

                rs : Both Int Int
                rs =
                    { core = CoreDict.insert (size // 2) 1 base.core
                    , fast = FastDict.insert (size // 2) 1 base.fast
                    }
            in
            case function of
                Core ->
                    \_ -> ignore <| ls.core == rs.core

                Fast ->
                    \_ -> ignore <| FastDict.equals ls.fast rs.fast

        EqualsIsFast Best ->
            -- equals #3 - different size
            let
                ( ls, rs ) =
                    fromRatioOverlap size ( 3, 2 ) OverlapRandom
            in
            case function of
                Core ->
                    \_ -> ignore <| ls.core == rs.core

                Fast ->
                    \_ -> ignore <| FastDict.equals ls.fast rs.fast


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
            let
                generated : Both Int Int
                generated =
                    generate rsizeFixed
            in
            if rsizeFixed == rsize then
                generated

            else
                case FastDict.getMinKey generated.fast of
                    Just k ->
                        { core = CoreDict.remove k generated.core
                        , fast = FastDict.remove k generated.fast
                        }

                    Nothing ->
                        generated

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
                        -- Prevent referential identity
                        mapBoth (always identity) ls

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
    Just 10
