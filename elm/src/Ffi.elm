module Ffi exposing (getProperty)

import ConcurrentTask
import Json.Decode
import Json.Encode


getProperty : Json.Encode.Value -> Json.Encode.Value -> Json.Decode.Decoder result -> ConcurrentTask.ConcurrentTask x result
getProperty object name decoder =
    ConcurrentTask.define
        { function = "property:get"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "object", object )
                , ( "name", name )
                ]
        }
