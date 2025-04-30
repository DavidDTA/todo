module Endpoints exposing (Endpoints, get, initEndpoints, map)


type Endpoints a
    = Endpoints a


initEndpoints =
    Endpoints


get (Endpoints value) =
    value


map fn (Endpoints value) =
    Endpoints (fn value)
