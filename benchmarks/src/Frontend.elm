port module Frontend exposing (main)

import Codec exposing (Value)
import FastBenchmark.Frontend
import ToBenchmark exposing (Function, Graph)


main : FastBenchmark.Frontend.Program Graph Function
main =
    FastBenchmark.Frontend.app ToBenchmark.config ports


ports : FastBenchmark.Frontend.Ports msg
ports =
    { terminateAll = terminateAll
    , fromBackend = fromBackend
    , toBackend = toBackend
    }


port terminateAll : {} -> Cmd msg


port fromBackend : ({ index : Int, data : Value } -> msg) -> Sub msg


port toBackend : { index : Int, data : Value } -> Cmd msg
