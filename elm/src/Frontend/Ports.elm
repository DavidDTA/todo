port module Frontend.Ports exposing (callMethod)

import Json.Decode


port callMethod :
    { object : Json.Decode.Value
    , methodName : String
    , args : List Json.Decode.Value
    }
    -> Cmd msg
