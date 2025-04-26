port module Backend.Interop exposing (Request, Resolver, attemptTask, receiveRequests, receiveTaskProgress, sendResponse, sendTaskError, sendTypescriptHandoff)

import ConcurrentTask
import Json.Decode


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


port taskErrors : () -> Cmd msg


type Request
    = Request Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


receiveRequests tag =
    requests
        (\{ request, resolver } -> tag { request = Request request, resolver = Resolver resolver })


sendTypescriptHandoff { request, resolver } =
    case request of
        Request jsRequest ->
            case resolver of
                Resolver jsResolver ->
                    typescriptHandoffs { request = jsRequest, resolver = jsResolver }


sendResponse { resolver, status, body } =
    case resolver of
        Resolver jsResolver ->
            responses
                { resolver = jsResolver
                , status = status
                , body = body
                }


sendTaskError =
    taskErrors ()


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
