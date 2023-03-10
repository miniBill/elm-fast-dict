module Intersect exposing (intersect)

import Internal exposing (Dict(..), InnerDict(..), VisitQueue)
import ListWithLength exposing (ListWithLength)


intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect (Dict sz1 t1) (Dict sz2 t2) =
    if sz1 == 0 || sz2 == 0 then
        Dict 0 Leaf

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        intersectFromZipper
            ListWithLength.empty
            (Internal.unconsBiggest [ t1 ])
            (Internal.unconsBiggest [ t2 ])
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
                        intersectFromZipper dacc (Internal.unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if rkey > lkey then
                        intersectFromZipper dacc lleft (Internal.unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (Internal.unconsBiggest ltail) (Internal.unconsBiggest rtail)
