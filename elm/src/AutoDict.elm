module AutoDict exposing
    ( AutoDict
    , Key
    , empty
    , foldl
    , insert
    , isEmpty
    , remove
    , update
    )

import Dict
import SafeInt.Unchecked


type AutoDict v
    = AutoDict
        { next : List Float
        , dict : Dict.Dict (List Float) v
        }


type Key
    = Key (List Float)


empty =
    AutoDict
        { next = []
        , dict = Dict.empty
        }


insert v (AutoDict { next, dict }) =
    let
        newNext =
            incr next
    in
    { dict =
        AutoDict
            { next = newNext
            , dict = Dict.insert newNext v dict
            }
    , key = Key newNext
    }


remove (Key k) (AutoDict { next, dict }) =
    AutoDict
        { next = next
        , dict = Dict.remove k dict
        }


update (Key k) fn (AutoDict { next, dict }) =
    AutoDict
        { next = next
        , dict = Dict.update k fn dict
        }


isEmpty (AutoDict { dict }) =
    Dict.isEmpty dict


foldl fn init (AutoDict { dict }) =
    Dict.foldl (\k v acc -> fn (Key k) v acc) init dict


incr k =
    case k of
        [] ->
            [ SafeInt.Unchecked.minValue ]

        head :: tail ->
            if head == SafeInt.Unchecked.maxValue then
                SafeInt.Unchecked.minValue :: incr tail

            else
                (head + 1) :: tail
