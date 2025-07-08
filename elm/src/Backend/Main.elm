module Backend.Main exposing (main)

import Api
import Backend.Interop
import Backend.Storage
import ConcurrentTask
import Endpoint
import Json.Decode
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
            ( model, Backend.Interop.sendError (expectedErrorToString error) )

        TaskCompleted (ConcurrentTask.UnexpectedError error) ->
            ( model, Backend.Interop.sendError (unexpectedErrorToString error) )

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
                                Backend.Interop.logError (expectedErrorToString error)
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


expectedErrorToString error =
    case error of
        Backend.Interop.JsException { message } ->
            message

        Backend.Interop.ResponseDecoderFailure decoderError ->
            Json.Decode.errorToString decoderError

        Backend.Interop.MultipleErrors errors ->
            errors
                |> List.map expectedErrorToString
                |> String.join "\n"


unexpectedErrorToString _ =
    "unexpected error"


handlers =
    Endpoint.handlers Backend.Interop.getLegacyResponse
        |> Endpoint.addHandler Api.priorities
            (Backend.Interop.withKv
                (\kv ->
                    Backend.Storage.getPriorities kv
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
                                            "index-unauthenticated.html"

                                        Just UniversalAuthentication ->
                                            "index-authenticated.html"
                                }
                        )
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


consts =
    { env =
        { token = "TOKEN"
        }
    , cookie =
        { token = "__Host-d"
        }
    }
