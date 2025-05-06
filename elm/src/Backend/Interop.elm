port module Backend.Interop exposing
    ( Request
    , Resolver
    , Response
    , attemptTask
    , getCookie
    , getEnvironment
    , getFileResponse
    , getLegacyResponse
    , getMethod
    , getResponse
    , getUrl
    , receiveRequests
    , receiveTaskProgress
    , sendResponse
    , sendTaskError
    )

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


port responses :
    { response : Json.Decode.Value
    , resolver : Json.Decode.Value
    }
    -> Cmd msg


port taskRequests : Json.Decode.Value -> Cmd msg


port taskResponses : (Json.Decode.Value -> msg) -> Sub msg


port taskErrors : String -> Cmd msg


type Request
    = Request Json.Decode.Value


type Response
    = Response Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


receiveRequests tag =
    requests
        (\{ request, resolver } -> tag { request = Request request, resolver = Resolver resolver })


sendResponse (Response response) (Resolver resolver) =
    responses
        { response = response
        , resolver = resolver
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


getResponse { status, body } =
    ConcurrentTask.define
        { function = "resp:get"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Response Json.Decode.value)
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "status", Json.Encode.int status )
                , ( "body", Json.Encode.string body )
                ]
        }


getFileResponse { request, filename } =
    ConcurrentTask.define
        { function = "resp:file"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Response Json.Decode.value)
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "request"
                  , case request of
                        Request jsRequest ->
                            jsRequest
                  )
                , ( "filename", Json.Encode.string filename )
                ]
        }


getLegacyResponse (Request request) =
    ConcurrentTask.define
        { function = "resp:legacy"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Response Json.Decode.value)
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }
