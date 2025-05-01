module Backend.Main exposing (main)

import Api
import Backend.Interop
import ConcurrentTask
import Endpoints
import Platform
import Url


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg Never { response : Response, resolver : Backend.Interop.Resolver }
    }


type Msg
    = NewRequest
        { request : Backend.Interop.Request
        , resolver : Backend.Interop.Resolver
        }
    | TaskProgress ( ConcurrentTask.Pool Msg Never { response : Response, resolver : Backend.Interop.Resolver }, Cmd Msg )
    | TaskCompleted (ConcurrentTask.Response Never { response : Response, resolver : Backend.Interop.Resolver })


type Authentication
    = UniversalAuthentication


type Response
    = Respond
        { status : Int
        , body : String
        }
    | PassToTypescript
        { request : Backend.Interop.Request
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

        TaskCompleted (ConcurrentTask.Success { resolver, response }) ->
            case response of
                Respond { status, body } ->
                    ( model, Backend.Interop.sendResponse { status = status, body = body, resolver = resolver } )

                PassToTypescript { request, isAuthenticated } ->
                    ( model, Backend.Interop.sendTypescriptHandoff { request = request, resolver = resolver, isAuthenticated = isAuthenticated } )

        NewRequest { request, resolver } ->
            let
                ( pool, cmd ) =
                    Endpoints.get endpoints request
                        |> ConcurrentTask.map (\response -> { resolver = resolver, response = response })
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
            ConcurrentTask.succeed (PassToTypescript { request = request, isAuthenticated = auth == Just UniversalAuthentication })
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


consts =
    { env =
        { token = "TOKEN"
        }
    , cookie =
        { token = "__Host-d"
        }
    }
