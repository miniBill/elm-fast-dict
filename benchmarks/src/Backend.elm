port module Backend exposing (main)

import Codec exposing (Value)
import FastBenchmark.Backend
import ToBenchmark exposing (Function, Graph)


main : FastBenchmark.Backend.Program Graph Function
main =
    FastBenchmark.Backend.app ToBenchmark.config ports


ports : FastBenchmark.Backend.Ports msg
ports =
    { fromFrontend = fromFrontend
    , toFrontend = toFrontend
    }


port fromFrontend : (Value -> msg) -> Sub msg


port toFrontend : Value -> Cmd msg
