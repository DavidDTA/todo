module Endpoint exposing
    ( Handlers
    , addHandler
    , get
    , getHandler
    , handlers
    , mapHandlers
    , post
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
        , request_ : request
        , response : response
        }


get =
    endpoint "GET"


post =
    endpoint "POST"


endpoint method path request_ response =
    Endpoint
        { method = method
        , path = path
        , request_ = request_ method path
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
