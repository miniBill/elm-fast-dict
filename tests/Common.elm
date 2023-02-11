module Common exposing (expectEqual)

import Dict as CoreDict
import Expect exposing (Expectation)
import FastDict as Dict exposing (Dict)


expectEqual : Dict comparable v -> Dict comparable v -> Expectation
expectEqual expected actual =
    actual
        |> Dict.toList
        |> CoreDict.fromList
        |> Expect.equalDicts (CoreDict.fromList <| Dict.toList expected)
