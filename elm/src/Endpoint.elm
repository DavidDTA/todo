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


type Endpoint att a
    = Endpoint
        { method : String
        , pathComponents : List String
        , responseDecoder : Json.Decode.Decoder a
        }


get : List String -> Json.Decode.Decoder a -> Endpoint { request : Available } a
get pathComponents responseDecoder =
    Endpoint
        { method = "GET"
        , pathComponents = pathComponents
        , responseDecoder = responseDecoder
        }


request : Endpoint { att | request : Available } a -> (Result Http.Error a -> b) -> Cmd b
request (Endpoint { method, pathComponents, responseDecoder }) tag =
    Http.request
        { method = method
        , headers = []
        , url = "/" ++ String.join "/" (List.map Url.percentEncode pathComponents)
        , body = Http.emptyBody
        , expect = Http.expectJson tag responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type Handlers a
    = Handlers
        { fallback : a
        , registered : Dict.Dict (List String) a
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
