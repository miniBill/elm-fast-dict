module Intersect exposing (intersect)

{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}

import Internal exposing (Dict(..), InnerDict(..), VisitQueue, unconsBiggest, unconsBiggestWhileDroppingGT)
import ListWithLength exposing (ListWithLength)


intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect (Dict sz1 t1) (Dict sz2 t2) =
    if sz1 == 0 || sz2 == 0 then
        Dict 0 Leaf

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        intersectFromZipper
            ListWithLength.empty
            (unconsBiggest [ t1 ])
            (unconsBiggest [ t2 ])
            |> Internal.fromSortedList


intersectFromZipper : ListWithLength ( comparable, v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> ListWithLength ( comparable, v )
intersectFromZipper dacc lleft rleft =
    case lleft of
        Nothing ->
            dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    dacc

                Just ( rkey, _, rtail ) ->
                    if lkey > rkey then
                        intersectFromZipper dacc (unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if lkey < rkey then
                        intersectFromZipper dacc lleft (unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (unconsBiggest ltail) (unconsBiggest rtail)
