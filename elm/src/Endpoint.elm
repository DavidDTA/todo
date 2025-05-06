module Endpoint exposing
    ( Handlers
    , get
    , getHandler
    , handlers
    , mapHandlers
    , request
    )

import Dict
import Http
import Json.Decode
import List
import String
import Url


type Available
    = Available


type Endpoint request response
    = Endpoint
        { method : String
        , pathComponents : List String
        , request_ : request
        , response : response
        }


get =
    endpoint "GET"


endpoint method pathComponents request_ response =
    Endpoint
        { method = method
        , pathComponents = pathComponents
        , request_ = request_ method pathComponents
        , response = response
        }


request (Endpoint { request_ }) =
    request_


type Handlers response
    = Handlers
        { fallback : response
        , registered : Dict.Dict (List String) response
        }


handlers fallback =
    Handlers { fallback = fallback, registered = Dict.empty }


getHandler method pathSegments (Handlers { fallback, registered }) =
    Dict.get (method :: pathSegments) registered
        |> Maybe.withDefault fallback


mapHandlers fn (Handlers { fallback, registered }) =
    Handlers
        { fallback = fn fallback
        , registered = Dict.map (always fn) registered
        }
