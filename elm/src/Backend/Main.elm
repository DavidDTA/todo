port module Backend.Main exposing (main)

import Json.Decode
import Platform


port requests :
    ({ request : Json.Decode.Value
     , resolve : Json.Decode.Value
     }
     -> msg
    )
    -> Sub msg


port typescriptHandler :
    { request : Json.Decode.Value
    , resolve : Json.Decode.Value
    }
    -> Cmd msg


type Msg
    = NewRequest
        { request : Json.Decode.Value
        , resolve : Json.Decode.Value
        }


main =
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update = update
        , subscriptions = always (requests NewRequest)
        }


update msg () =
    case msg of
        NewRequest { request, resolve } ->
            ( (), typescriptHandler { request = request, resolve = resolve } )
