module Endpoint exposing (get, request)

import Http
import Json.Decode
import List
import String
import Url


type Endpoint a
    = Endpoint
        { method : String
        , pathComponents : List String
        , responseDecoder : Json.Decode.Decoder a
        }


get pathComponents responseDecoder =
    Endpoint
        { method = "GET"
        , pathComponents = pathComponents
        , responseDecoder = responseDecoder
        }


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
