module Internal exposing (Dict(..), InnerDict(..), NColor(..))

-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


type InnerDict k v
    = InnerNode NColor k v (InnerDict k v) (InnerDict k v)
    | Leaf


type Dict k v
    = Dict Int (InnerDict k v)
