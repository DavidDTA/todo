port module Backend.Main exposing (main)

import ConcurrentTask
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


port responses :
    { resolve : Json.Decode.Value
    , status : Int
    , body : String
    }
    -> Cmd msg


port taskSend : Json.Decode.Value -> Cmd msg


port taskReceive : (Json.Decode.Value -> msg) -> Sub msg


port taskErrors : () -> Cmd msg


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Never TaskSuccess
    }


type Msg
    = NewRequest
        { request : Json.Decode.Value
        , resolve : Json.Decode.Value
        }
    | TaskProgress ( ConcurrentTask.Pool Msg Never TaskSuccess, Cmd Msg )
    | TaskCompleted (ConcurrentTask.Response Never TaskSuccess)


type TaskSuccess
    = Respond
        { resolve : Json.Decode.Value
        , status : Int
        , body : String
        }
    | PassToTypescript
        { request : Json.Decode.Value
        , resolve : Json.Decode.Value
        }


main =
    Platform.worker
        { init =
            \() ->
                ( { taskPool = ConcurrentTask.pool
                  }
                , Cmd.none
                )
        , update = update
        , subscriptions = subscriptions
        }


update msg model =
    case msg of
        TaskProgress ( pool, cmd ) ->
            ( { model | taskPool = pool }, cmd )

        TaskCompleted (ConcurrentTask.Error error) ->
            never error

        TaskCompleted (ConcurrentTask.UnexpectedError _) ->
            ( model, taskErrors () )

        TaskCompleted (ConcurrentTask.Success success) ->
            case success of
                Respond result ->
                    ( model, responses result )

                PassToTypescript result ->
                    ( model, typescriptHandler result )

        NewRequest { request, resolve } ->
            let
                ( pool, cmd ) =
                    ConcurrentTask.attempt
                        { pool = model.taskPool
                        , send = taskSend
                        , onComplete = TaskCompleted
                        }
                        (ConcurrentTask.succeed (PassToTypescript { request = request, resolve = resolve }))
            in
            ( { model | taskPool = pool }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ConcurrentTask.onProgress
            { send = taskSend
            , receive = taskReceive
            , onProgress = TaskProgress
            }
            model.taskPool
        , requests NewRequest
        ]
