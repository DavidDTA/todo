port module Backend.Interop exposing (Request, Resolver, attemptTask, getCookie, getEnvironment, getMethod, getUrl, receiveRequests, receiveTaskProgress, sendResponse, sendTaskError, sendTypescriptHandoff)

import ConcurrentTask
import Json.Decode
import Json.Encode


port requests :
    ({ request : Json.Decode.Value
     , resolver : Json.Decode.Value
     }
     -> msg
    )
    -> Sub msg


port typescriptHandoffs :
    { request : Json.Decode.Value
    , resolver : Json.Decode.Value
    , isAuthenticated : Bool
    }
    -> Cmd msg


port responses :
    { resolver : Json.Decode.Value
    , status : Int
    , body : String
    }
    -> Cmd msg


port taskRequests : Json.Decode.Value -> Cmd msg


port taskResponses : (Json.Decode.Value -> msg) -> Sub msg


port taskErrors : String -> Cmd msg


type Request
    = Request Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


receiveRequests tag =
    requests
        (\{ request, resolver } -> tag { request = Request request, resolver = Resolver resolver })


sendTypescriptHandoff { request, resolver, isAuthenticated } =
    case request of
        Request jsRequest ->
            case resolver of
                Resolver jsResolver ->
                    typescriptHandoffs { request = jsRequest, resolver = jsResolver, isAuthenticated = isAuthenticated }


sendResponse { resolver, status, body } =
    case resolver of
        Resolver jsResolver ->
            responses
                { resolver = jsResolver
                , status = status
                , body = body
                }


sendTaskError =
    taskErrors


receiveTaskProgress onProgress pool =
    ConcurrentTask.onProgress
        { send = taskRequests
        , receive = taskResponses
        , onProgress = onProgress
        }
        pool


attemptTask onComplete pool task =
    ConcurrentTask.attempt
        { pool = pool
        , send = taskRequests
        , onComplete = onComplete
        }
        task


getEnvironment key =
    ConcurrentTask.define
        { function = "env:get"
        , expect = ConcurrentTask.expectJson (Json.Decode.nullable Json.Decode.string)
        , errors = ConcurrentTask.expectNoErrors
        , args = Json.Encode.string key
        }


getMethod (Request request) =
    ConcurrentTask.define
        { function = "req:getMethod"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


getUrl (Request request) =
    ConcurrentTask.define
        { function = "req:getUrl"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


getCookie key (Request request) =
    ConcurrentTask.define
        { function = "req:getCookie"
        , expect = ConcurrentTask.expectJson (Json.Decode.nullable Json.Decode.string)
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "request", request ), ( "key", Json.Encode.string key ) ]
        }
