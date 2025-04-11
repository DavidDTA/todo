module KeyDict exposing (KeyDict, define)

import Dict


type KeyDict k comparable v
    = KeyDict (Dict.Dict comparable v)


type alias Ops k comparable v a b result =
    { empty : KeyDict k comparable v
    , singleton : k -> v -> KeyDict k comparable v
    , insert : k -> v -> KeyDict k comparable v -> KeyDict k comparable v
    , update : k -> (Maybe v -> Maybe v) -> KeyDict k comparable v -> KeyDict k comparable v
    , remove : k -> KeyDict k comparable v -> KeyDict k comparable v
    , isEmpty : KeyDict k comparable v -> Bool
    , member : k -> KeyDict k comparable v -> Bool
    , get : k -> KeyDict k comparable v -> Maybe v
    , size : KeyDict k comparable v -> Int
    , keys : KeyDict k comparable v -> List k
    , values : KeyDict k comparable v -> List v
    , toList : KeyDict k comparable v -> List ( k, v )
    , fromList : List ( k, v ) -> KeyDict k comparable v
    , map : (k -> a -> b) -> KeyDict k comparable a -> KeyDict k comparable b
    , foldl : (k -> v -> b -> b) -> b -> KeyDict k comparable v -> b
    , foldr : (k -> v -> b -> b) -> b -> KeyDict k comparable v -> b
    , filter : (k -> v -> Bool) -> KeyDict k comparable v -> KeyDict k comparable v
    , partition : (k -> v -> Bool) -> KeyDict k comparable v -> ( KeyDict k comparable v, KeyDict k comparable v )
    , union : KeyDict k comparable v -> KeyDict k comparable v -> KeyDict k comparable v
    , intersect : KeyDict k comparable v -> KeyDict k comparable v -> KeyDict k comparable v
    , diff : KeyDict k comparable a -> KeyDict k comparable b -> KeyDict k comparable a
    , merge : (k -> a -> result -> result) -> (k -> a -> b -> result -> result) -> (k -> b -> result -> result) -> KeyDict k comparable a -> KeyDict k comparable b -> result -> result
    }


define : (comparable -> k) -> (k -> comparable) -> (Ops k comparable v a b result -> op) -> op
define wrap unwrap op =
    op
        { empty = KeyDict Dict.empty
        , singleton = \k v -> KeyDict (Dict.singleton (unwrap k) v)
        , insert = \k v (KeyDict dict) -> KeyDict (Dict.insert (unwrap k) v dict)
        , update = \k fn (KeyDict dict) -> KeyDict (Dict.update (unwrap k) fn dict)
        , remove = \k (KeyDict dict) -> KeyDict (Dict.remove (unwrap k) dict)
        , isEmpty = \(KeyDict dict) -> Dict.isEmpty dict
        , member = \k (KeyDict dict) -> Dict.member (unwrap k) dict
        , get = \k (KeyDict dict) -> Dict.get (unwrap k) dict
        , size = \(KeyDict dict) -> Dict.size dict
        , keys = \(KeyDict dict) -> List.map wrap (Dict.keys dict)
        , values = \(KeyDict dict) -> Dict.values dict
        , toList = \(KeyDict dict) -> List.map (\( k, v ) -> ( wrap k, v )) (Dict.toList dict)
        , fromList = \list -> KeyDict (Dict.fromList (List.map (\( k, v ) -> ( unwrap k, v )) list))
        , map = \fn (KeyDict dict) -> KeyDict (Dict.map (\k -> fn (wrap k)) dict)
        , foldl = \acc init (KeyDict dict) -> Dict.foldl (\k -> acc (wrap k)) init dict
        , foldr = \acc init (KeyDict dict) -> Dict.foldr (\k -> acc (wrap k)) init dict
        , filter = \pred (KeyDict dict) -> KeyDict (Dict.filter (\k -> pred (wrap k)) dict)
        , partition =
            \pred (KeyDict dict) ->
                case Dict.partition (\k -> pred (wrap k)) dict of
                    ( left, right ) ->
                        ( KeyDict left, KeyDict right )
        , union = \(KeyDict left) (KeyDict right) -> KeyDict (Dict.union left right)
        , intersect = \(KeyDict left) (KeyDict right) -> KeyDict (Dict.intersect left right)
        , diff = \(KeyDict left) (KeyDict right) -> KeyDict (Dict.diff left right)
        , merge = \accLeft accBoth accRight (KeyDict left) (KeyDict right) init -> Dict.merge (\k -> accLeft (wrap k)) (\k -> accBoth (wrap k)) (\k -> accRight (wrap k)) left right init
        }
