module Endpoint exposing
    ( Endpoint
    , Handlers
    , addHandler
    , endpoint
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
        , path : List String
        , request : request
        , response : response
        }


endpoint : String -> List String -> { request : String -> List String -> req, response : res } -> Endpoint req res
endpoint method path r =
    Endpoint
        { method = method
        , path = path
        , request = r.request method path
        , response = r.response
        }


request (Endpoint e) =
    e.request


type Handlers response
    = Handlers
        { fallback : response
        , registered : Dict.Dict (List String) response
        }


handlers fallback =
    Handlers { fallback = fallback, registered = Dict.empty }


addHandler : Endpoint req (impl -> handler) -> impl -> Handlers handler -> Handlers handler
addHandler (Endpoint { method, path, response }) handler (Handlers handlers_) =
    Handlers
        { handlers_
            | registered = Dict.insert (method :: path) (response handler) handlers_.registered
        }


getHandler method path (Handlers { fallback, registered }) =
    Dict.get (method :: path) registered
        |> Maybe.withDefault fallback


mapHandlers fn (Handlers { fallback, registered }) =
    Handlers
        { fallback = fn fallback
        , registered = Dict.map (always fn) registered
        }
