module Endpoint exposing (get)

import Http
import List
import String
import Url


get pathComponents tag jsonDecoder =
    Http.get
        { url = "/" ++ String.join "/" (List.map Url.percentEncode pathComponents)
        , expect = Http.expectJson tag jsonDecoder
        }
