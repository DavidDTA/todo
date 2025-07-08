port module Backend.Interop exposing
    ( Error(..)
    , Kv
    , Request
    , Resolver
    , Response
    , attemptTask
    , getBody
    , getCookie
    , getEnvironment
    , getFileResponse
    , getLegacyResponse
    , getMethod
    , getResponse
    , getUrl
    , kvGet
    , logError
    , receiveRequests
    , receiveTaskProgress
    , resolveRequest
    , sendError
    , withKv
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


port taskRequests : Json.Decode.Value -> Cmd msg


port taskResponses : (Json.Decode.Value -> msg) -> Sub msg


port errors : String -> Cmd msg


type Kv
    = Kv Json.Decode.Value


type Request
    = Request Json.Decode.Value


type Response
    = Response Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


type Error
    = JsException
        { message : String
        , raw : Json.Decode.Value
        }
    | ResponseDecoderFailure Json.Decode.Error
    | MultipleErrors (List Error)


receiveRequests tag =
    requests
        (\{ request, resolver } -> tag { request = Request request, resolver = Resolver resolver })


sendError =
    errors


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
    defineTask
        { function = "env:get"
        , expect = ConcurrentTask.expectJson (Json.Decode.nullable Json.Decode.string)
        , errors = ConcurrentTask.expectNoErrors
        , args = Json.Encode.string key
        }


logError message =
    defineTask
        { function = "log:error"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args = Json.Encode.string message
        }


closeKv (Kv kv) =
    defineTask
        { function = "kv:close"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args = kv
        }


kvGet (Kv kv) key decoder =
    defineTask
        { function = "kv:get"
        , expect =
            ConcurrentTask.expectJson
                (Json.Decode.oneOf
                    [ Json.Decode.field "versionstamp" (Json.Decode.null Nothing)
                    , Json.Decode.map2 (\versionstamp value -> { versionstamp = versionstamp, value = value })
                        (Json.Decode.field "versionstamp" Json.Decode.string)
                        (Json.Decode.field "value" decoder)
                        |> Json.Decode.map Just
                    ]
                )
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "kv", kv )
                , ( "key", Json.Encode.list Json.Encode.string key )
                ]
        }


openKv =
    defineTask
        { function = "kv:open"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Kv Json.Decode.value)
        , errors = ConcurrentTask.expectNoErrors
        , args = Json.Encode.null
        }


withKv task =
    openKv
        |> ConcurrentTask.andThen
            (\kv ->
                task kv
                    |> ConcurrentTask.onError
                        (\error ->
                            closeKv kv
                                |> ConcurrentTask.onError (\error2 -> ConcurrentTask.fail (MultipleErrors [ error, error2 ]))
                                |> ConcurrentTask.andThen (\_ -> ConcurrentTask.fail error)
                        )
                    |> ConcurrentTask.andThen
                        (\result ->
                            closeKv kv
                                |> ConcurrentTask.return result
                        )
            )


getMethod (Request request) =
    defineTask
        { function = "req:getMethod"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


getUrl (Request request) =
    defineTask
        { function = "req:getUrl"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


getCookie key (Request request) =
    defineTask
        { function = "req:getCookie"
        , expect = ConcurrentTask.expectJson (Json.Decode.nullable Json.Decode.string)
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "request", request ), ( "key", Json.Encode.string key ) ]
        }


getBody (Request request) =
    defineTask
        { function = "req:getBody"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


resolveRequest (Resolver resolver) (Response response) =
    defineTask
        { function = "req:resolve"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "resolver", resolver )
                , ( "response", response )
                ]
        }


getResponse { status, body } =
    defineTask
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
    defineTask
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
    defineTask
        { function = "resp:legacy"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Response Json.Decode.value)
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }


defineTask definition =
    ConcurrentTask.define definition
        |> ConcurrentTask.onJsException (\e -> ConcurrentTask.fail (JsException e))
        |> ConcurrentTask.onResponseDecoderFailure (\e -> ConcurrentTask.fail (ResponseDecoderFailure e))
