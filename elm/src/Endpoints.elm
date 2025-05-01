module Endpoints exposing (Endpoints, get, initEndpoints, map)

import Dict


type Endpoints a
    = Endpoints
        { fallback : a
        , handlers : Dict.Dict (List String) a
        }


initEndpoints fallback =
    Endpoints { fallback = fallback, handlers = Dict.empty }


get method pathSegments (Endpoints { fallback, handlers }) =
    Dict.get (method :: pathSegments) handlers
        |> Maybe.withDefault fallback


map fn (Endpoints { fallback, handlers }) =
    Endpoints
        { fallback = fn fallback
        , handlers = Dict.map (always fn) handlers
        }
