module Backend.Main exposing (main)

import Api
import Backend.Id
import Backend.Interop
import Backend.Storage
import ConcurrentTask
import ConcurrentTask.Random
import Endpoint
import Errors
import Json.Decode
import Json.Encode
import Platform
import Url


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Never ()
    }


type Msg
    = NewRequest
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        }
    | TaskProgress
        ( ConcurrentTask.Pool Msg Never ()
        , Cmd Msg
        )
    | TaskCompleted (ConcurrentTask.Response Never ())


type Authentication
    = UniversalAuthentication


type Error
    = InternalError { log : List Json.Decode.Value }
    | NotAuthorized { log : List Json.Decode.Value }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TaskProgress ( pool, cmd ) ->
            ( { model | taskPool = pool }, cmd )

        TaskCompleted (ConcurrentTask.Error error) ->
            never error

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
                                    case Endpoint.splitPath path of
                                        Nothing ->
                                            Api.badRequest

                                        Just pathSegments ->
                                            Endpoint.getHandler method
                                                pathSegments
                                                handlers
                                                request
                        )
                        (Backend.Interop.getMethod request)
                        (Backend.Interop.getUrl request)
                        |> ConcurrentTask.andThen identity
                        |> ConcurrentTask.onError handleError
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


handleError error =
    case error of
        NotAuthorized { log } ->
            Backend.Interop.logError log
                |> ConcurrentTask.andThenDo Api.forbidden

        InternalError { log } ->
            Backend.Interop.logError log
                |> ConcurrentTask.andThenDo Api.internalServerError


formatUnexpectedError unexpectedError =
    case unexpectedError of
        ConcurrentTask.UnhandledJsException { function, message, raw } ->
            [ Json.Encode.string ("Unhandled JS exception in " ++ function)
            , Json.Encode.string message
            , raw
            ]

        ConcurrentTask.ResponseDecoderFailure { function, error } ->
            [ Json.Encode.string ("Response decoder failure in " ++ function)
            , Json.Encode.string (Json.Decode.errorToString error)
            ]

        ConcurrentTask.ErrorsDecoderFailure { function, error } ->
            [ Json.Encode.string ("Errors decoder failure in " ++ function)
            , Json.Encode.string (Json.Decode.errorToString error)
            ]

        ConcurrentTask.MissingFunction error ->
            [ Json.Encode.string "Missing function"
            , Json.Encode.string error
            ]

        ConcurrentTask.InternalError error ->
            [ Json.Encode.string "ConcurrentTask internal error"
            , Json.Encode.string error
            ]


handlers =
    Endpoint.handlers (\request -> Backend.Interop.getLegacyResponse request)
        |> Endpoint.addHandler Api.waypoints getWaypoints
        |> Endpoint.addHandler Api.waypointAdd
            (\{ text } ->
                Backend.Interop.withKv
                    (\kv ->
                        transact kv
                            (\op ->
                                Backend.Id.generate Api.WaypointId
                                    |> ConcurrentTask.andThen
                                        (\id ->
                                            Backend.Storage.waypointAdd op
                                                id
                                                { text = text
                                                }
                                                |> ConcurrentTask.return
                                                    { id = id
                                                    , waypoint =
                                                        { completed = False
                                                        , priority = Nothing
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
        |> Endpoint.addHandler Api.waypointDelete
            (\{ id } ->
                Backend.Interop.withKv
                    (\kv ->
                        transact kv
                            (\op ->
                                Backend.Storage.waypointDelete op id
                                    |> ConcurrentTask.return {}
                            )
                            |> ConcurrentTask.map .result
                    )
            )
        |> Endpoint.addHandler Api.waypointSetCompleted waypointSetCompleted
        |> Endpoint.addHandler Api.waypointSetPriority waypointSetPriority
        |> Endpoint.addHandler Api.export
            (ConcurrentTask.map
                (\waypoints_ ->
                    { waypoints = waypoints_
                    }
                )
                getWaypoints
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
        |> Endpoint.addHandler Api.home frontend
        |> Endpoint.addHandler Api.detail frontend
        |> Endpoint.addHandler Api.appjs
            (\request ->
                Backend.Interop.getFileResponse
                    { request = request
                    , filename = "files/app.js"
                    }
            )
        |> Endpoint.addHandler Api.login (\request -> Backend.Interop.getLegacyResponse request)


getWaypoints =
    Backend.Interop.withKv
        (\kv ->
            Backend.Storage.waypointsList kv
        )
        |> ConcurrentTask.map
            (List.map
                (\{ id, waypoint } ->
                    { id = id
                    , waypoint =
                        { text = waypoint.text
                        , completed = waypoint.completed
                        , priority = waypoint.priority
                        , url = Nothing
                        , requires = []
                        , requiredBy = []
                        }
                    }
                )
            )
        |> ConcurrentTask.mapError
            (\errors ->
                InternalError
                    { log =
                        Errors.toList errors
                            |> List.map
                                (\error ->
                                    case error of
                                        Backend.Storage.WaypointsListUnexpectedKey log ->
                                            log

                                        Backend.Storage.WaypointsListDecodeError log ->
                                            log
                                )
                            |> List.map (Json.Encode.list identity)
                    }
            )


waypointSetCompleted { id, completed } =
    Backend.Interop.withKv
        (\kv ->
            transact kv
                (\op ->
                    Backend.Storage.waypointSetCompleted kv op id completed
                        |> ConcurrentTask.return {}
                )
                |> ConcurrentTask.map .result
        )
        |> ConcurrentTask.mapError
            (\error ->
                case error of
                    Backend.Storage.WaypointSetCompletedMissingWaypoint log ->
                        NotAuthorized { log = log }

                    Backend.Storage.WaypointSetCompletedDecodeError log ->
                        InternalError { log = log }
            )


waypointSetPriority { id, priority } =
    Backend.Interop.withKv
        (\kv ->
            transact kv
                (\op ->
                    Backend.Storage.waypointSetPriority kv op id priority
                        |> ConcurrentTask.return {}
                )
                |> ConcurrentTask.map .result
        )
        |> ConcurrentTask.mapError
            (\error ->
                case error of
                    Backend.Storage.WaypointSetPriorityMissingWaypoint log ->
                        NotAuthorized { log = log }

                    Backend.Storage.WaypointSetPriorityDecodeError log ->
                        InternalError { log = log }
            )


frontend request =
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
        { token = "__Host-Http-a"
        }
    }
