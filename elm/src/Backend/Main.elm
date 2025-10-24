module Backend.Main exposing (main)

import Api
import Backend.Id
import Backend.Interop
import Backend.Storage
import ConcurrentTask
import ConcurrentTask.Random
import Endpoint
import Json.Decode
import Json.Encode
import Platform
import Url


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Backend.Interop.Error ()
    }


type Msg
    = NewRequest
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        }
    | TaskProgress
        ( ConcurrentTask.Pool Msg Backend.Interop.Error ()
        , Cmd Msg
        )
    | TaskCompleted (ConcurrentTask.Response Backend.Interop.Error ())


type Authentication
    = UniversalAuthentication


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
            ( model
            , formatExpectedError error
                |> List.map Backend.Interop.sendError
                |> Cmd.batch
            )

        TaskCompleted (ConcurrentTask.UnexpectedError error) ->
            ( model, Backend.Interop.sendError (formatUnexpectedError error) )

        TaskCompleted (ConcurrentTask.Success ()) ->
            ( model, Cmd.none )

        NewRequest { request, resolver } ->
            let
                ( pool, cmd ) =
                    ConcurrentTask.map2
                        (\method url ->
                            case Url.fromString url of
                                Nothing ->
                                    Api.badRequest

                                Just { path } ->
                                    case String.split "/" path of
                                        [] ->
                                            Api.badRequest

                                        "" :: pathSegments ->
                                            let
                                                decodedPathSegments =
                                                    List.filterMap Url.percentDecode pathSegments
                                            in
                                            if List.length pathSegments == List.length decodedPathSegments then
                                                Endpoint.getHandler method decodedPathSegments handlers request

                                            else
                                                Api.badRequest

                                        _ ->
                                            Api.badRequest
                        )
                        (Backend.Interop.getMethod request)
                        (Backend.Interop.getUrl request)
                        |> ConcurrentTask.andThen identity
                        |> ConcurrentTask.onError
                            (\error ->
                                formatExpectedError error
                                    |> List.map Backend.Interop.logError
                                    |> ConcurrentTask.sequence
                                    |> ConcurrentTask.andThenDo Api.internalServerError
                            )
                        |> ConcurrentTask.andThen (\response -> Backend.Interop.resolveRequest resolver response)
                        |> Backend.Interop.attemptTask TaskCompleted model.taskPool
            in
            ( { model | taskPool = pool }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Backend.Interop.receiveTaskProgress TaskProgress model.taskPool
        , Backend.Interop.receiveRequests NewRequest
        ]


formatExpectedError error =
    case error of
        Backend.Interop.JsException { function, raw } ->
            [ { context = "JS exception in " ++ function
              , error = raw
              }
            ]

        Backend.Interop.ResponseDecoderFailure decoderError ->
            [ { context = "Response decoder failure in " ++ decoderError.function
              , error = Json.Encode.string (Json.Decode.errorToString decoderError.error)
              }
            ]

        Backend.Interop.MultipleErrors errors ->
            errors
                |> List.concatMap formatExpectedError


formatUnexpectedError unexpectedError =
    case unexpectedError of
        ConcurrentTask.UnhandledJsException { function, raw } ->
            { context = "Unhandled JS exception in " ++ function
            , error = raw
            }

        ConcurrentTask.ResponseDecoderFailure { function, error } ->
            { context = "Response decoder failure in " ++ function
            , error = Json.Encode.string (Json.Decode.errorToString error)
            }

        ConcurrentTask.ErrorsDecoderFailure { function, error } ->
            { context = "Errors decoder failure in " ++ function
            , error = Json.Encode.string (Json.Decode.errorToString error)
            }

        ConcurrentTask.MissingFunction error ->
            { context = "Missing function"
            , error = Json.Encode.string error
            }

        ConcurrentTask.InternalError error ->
            { context = "ConcurrentTask internal error"
            , error = Json.Encode.string error
            }


handlers =
    Endpoint.handlers Backend.Interop.getLegacyResponse
        |> Endpoint.addHandler Api.priorities
            (Backend.Interop.withKv
                (\kv ->
                    Backend.Storage.getPriorities kv
                )
            )
        |> Endpoint.addHandler Api.addWaypoint
            (\{ text } ->
                Backend.Interop.withKv
                    (\kv ->
                        transact kv
                            (\op ->
                                Backend.Id.generate Api.WaypointId
                                    |> ConcurrentTask.andThen
                                        (\id ->
                                            Backend.Storage.addWaypoint op
                                                id
                                                { text = text
                                                }
                                                |> ConcurrentTask.return
                                                    { id = id
                                                    , waypoint =
                                                        { completed = False
                                                        , requiredBy = []
                                                        , requires = []
                                                        , text = text
                                                        , url = Nothing
                                                        }
                                                    }
                                        )
                            )
                            |> ConcurrentTask.map .result
                    )
            )
        |> Endpoint.mapHandlers
            (\handler request ->
                getAuthentication request
                    |> ConcurrentTask.andThen
                        (\maybeAuth ->
                            case maybeAuth of
                                Nothing ->
                                    Api.forbidden

                                Just auth ->
                                    handler request
                        )
            )
        |> Endpoint.addHandler Api.home
            (\request ->
                getAuthentication request
                    |> ConcurrentTask.andThen
                        (\auth ->
                            Backend.Interop.getFileResponse
                                { request = request
                                , filename =
                                    case auth of
                                        Nothing ->
                                            "files/index-unauthenticated.html"

                                        Just UniversalAuthentication ->
                                            "files/index-authenticated.html"
                                }
                        )
            )
        |> Endpoint.addHandler Api.appjs
            (\request ->
                Backend.Interop.getFileResponse
                    { request = request
                    , filename = "files/app.js"
                    }
            )
        |> Endpoint.addHandler Api.login (always Backend.Interop.getLegacyResponse)


getAuthentication request =
    ConcurrentTask.map2
        (\envToken cookieToken ->
            if envToken == cookieToken then
                Just UniversalAuthentication

            else
                Nothing
        )
        (Backend.Interop.getEnvironment consts.env.token)
        (Backend.Interop.getCookie consts.cookie.token request)


transact kv transaction =
    Backend.Interop.kvAtomic kv
        |> ConcurrentTask.andThen
            (\op ->
                transaction op
                    |> ConcurrentTask.andThen
                        (\result ->
                            Backend.Interop.atomicOpCommit op
                                |> ConcurrentTask.andThen
                                    (\commitResult ->
                                        case commitResult of
                                            Ok versionstamp ->
                                                ConcurrentTask.succeed { versionstamp = versionstamp, result = result }

                                            Err () ->
                                                transact kv transaction
                                    )
                        )
            )


consts =
    { env =
        { token = "TOKEN"
        }
    , cookie =
        { token = "__Host-d"
        }
    }
