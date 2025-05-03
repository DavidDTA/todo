module Backend.Main exposing (main)

import Api
import Backend.Interop
import ConcurrentTask
import Endpoints
import Platform
import Url


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Never { response : Backend.Interop.Response, resolver : Backend.Interop.Resolver }
    }


type Msg
    = NewRequest
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        }
    | TaskProgress ( ConcurrentTask.Pool Msg Never { response : Backend.Interop.Response, resolver : Backend.Interop.Resolver }, Cmd Msg )
    | TaskCompleted (ConcurrentTask.Response Never { response : Backend.Interop.Response, resolver : Backend.Interop.Resolver })


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
            never error

        TaskCompleted (ConcurrentTask.UnexpectedError error) ->
            ( model, Backend.Interop.sendTaskError "" )

        TaskCompleted (ConcurrentTask.Success { response, resolver }) ->
            ( model, Backend.Interop.sendResponse response resolver )

        NewRequest { request, resolver } ->
            let
                ( pool, cmd ) =
                    ConcurrentTask.map2
                        (\method url ->
                            case Url.fromString url of
                                Nothing ->
                                    badRequest

                                Just { path } ->
                                    case String.split "/" path of
                                        [] ->
                                            badRequest

                                        "" :: pathSegments ->
                                            let
                                                decodedPathSegments =
                                                    List.filterMap Url.percentDecode pathSegments
                                            in
                                            if List.length pathSegments == List.length decodedPathSegments then
                                                Endpoints.get method decodedPathSegments endpoints request

                                            else
                                                badRequest

                                        _ ->
                                            badRequest
                        )
                        (Backend.Interop.getMethod request)
                        (Backend.Interop.getUrl request)
                        |> ConcurrentTask.andThen (ConcurrentTask.map (\response -> { resolver = resolver, response = response }))
                        |> Backend.Interop.attemptTask TaskCompleted model.taskPool
            in
            ( { model | taskPool = pool }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Backend.Interop.receiveTaskProgress TaskProgress model.taskPool
        , Backend.Interop.receiveRequests NewRequest
        ]


endpoints =
    Endpoints.initEndpoints
        (\auth request ->
            Backend.Interop.getLegacyResponse { request = request, isAuthenticated = auth == Just UniversalAuthentication }
        )
        |> Endpoints.map
            (\handler request ->
                ConcurrentTask.map2
                    (\envToken cookieToken ->
                        if envToken == cookieToken then
                            Just UniversalAuthentication

                        else
                            Nothing
                    )
                    (Backend.Interop.getEnvironment consts.env.token)
                    (Backend.Interop.getCookie consts.cookie.token request)
                    |> ConcurrentTask.andThen (\auth -> handler auth request)
            )


badRequest =
    Backend.Interop.getResponse { status = 400, body = "" }


consts =
    { env =
        { token = "TOKEN"
        }
    , cookie =
        { token = "__Host-d"
        }
    }
