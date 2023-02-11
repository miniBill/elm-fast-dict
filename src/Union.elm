module Union exposing (union)

import Internal exposing (Dict(..), VisitQueue)
import ListWithLength exposing (ListWithLength)


union : Dict comparable v -> Dict comparable v -> Dict comparable v
union ((Dict sz1 t1) as d1) ((Dict sz2 t2) as d2) =
    if sz1 == 0 then
        d2

    else if sz2 == 0 then
        d1

    else
        -- Now t1 and t2 are never leaves, so we have an invariant that queues never contain leaves
        unionFromZipper
            ListWithLength.empty
            (Internal.unconsBiggest [ t1 ])
            (Internal.unconsBiggest [ t2 ])
            |> Internal.fromSortedList


unionFromZipper : ListWithLength ( comparable, v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> Maybe ( comparable, v, VisitQueue comparable v ) -> ListWithLength ( comparable, v )
unionFromZipper dacc lleft rleft =
    case lleft of
        Nothing ->
            unwrap rleft dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    unwrap lleft dacc

                Just ( rkey, rvalue, rtail ) ->
                    if lkey > rkey then
                        unionFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (Internal.unconsBiggest ltail) rleft

                    else if rkey > lkey then
                        unionFromZipper (ListWithLength.cons ( rkey, rvalue ) dacc) lleft (Internal.unconsBiggest rtail)

                    else
                        unionFromZipper (ListWithLength.cons ( lkey, lvalue ) dacc) (Internal.unconsBiggest ltail) (Internal.unconsBiggest rtail)


unwrap : Maybe ( comparable, v, VisitQueue comparable v ) -> ListWithLength ( comparable, v ) -> ListWithLength ( comparable, v )
unwrap vacc dacc =
    case vacc of
        Nothing ->
            dacc

        Just ( key, value, rest ) ->
            unwrap (Internal.unconsBiggest rest) (ListWithLength.cons ( key, value ) dacc)
