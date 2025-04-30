module Backend.Main exposing (main)

import Api
import Backend.Interop
import ConcurrentTask
import Endpoints
import Platform


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Never TaskSuccess
    }


type Msg
    = NewRequest
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        }
    | TaskProgress ( ConcurrentTask.Pool Msg Never TaskSuccess, Cmd Msg )
    | TaskCompleted (ConcurrentTask.Response Never TaskSuccess)


type Authentication
    = UniversalAuthentication


type TaskSuccess
    = Respond
        { resolver : Backend.Interop.Resolver
        , status : Int
        , body : String
        }
    | PassToTypescript
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        , isAuthenticated : Bool
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

        TaskCompleted (ConcurrentTask.UnexpectedError error) ->
            ( model, Backend.Interop.sendTaskError "" )

        TaskCompleted (ConcurrentTask.Success success) ->
            case success of
                Respond result ->
                    ( model, Backend.Interop.sendResponse result )

                PassToTypescript result ->
                    ( model, Backend.Interop.sendTypescriptHandoff result )

        NewRequest { request, resolver } ->
            let
                ( pool, cmd ) =
                    Endpoints.get endpoints request resolver
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
        (\auth request resolver ->
            ConcurrentTask.succeed (PassToTypescript { request = request, resolver = resolver, isAuthenticated = auth == Just UniversalAuthentication })
        )
        |> Endpoints.map
            (\handler request resolver ->
                ConcurrentTask.map2
                    (\envToken cookieToken ->
                        if envToken == cookieToken then
                            Just UniversalAuthentication

                        else
                            Nothing
                    )
                    (Backend.Interop.getEnvironment consts.env.token)
                    (Backend.Interop.getCookie consts.cookie.token request)
                    |> ConcurrentTask.andThen (\auth -> handler auth request resolver)
            )


consts =
    { env =
        { token = "TOKEN"
        }
    , cookie =
        { token = "__Host-d"
        }
    }
