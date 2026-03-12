port module Backend.Interop exposing
    ( AtomicOperation
    , Error(..)
    , Kv
    , Request
    , Resolver
    , Response
    , atomicOpCheck
    , atomicOpCommit
    , atomicOpSet
    , attemptTask
    , getBody
    , getCookie
    , getEnvironment
    , getFileResponse
    , getLegacyResponse
    , getMethod
    , getRandom
    , getResponse
    , getUrl
    , kvAtomic
    , kvGet
    , logError
    , receiveRequests
    , receiveTaskProgress
    , resolveRequest
    , sendError
    , withKv
    )

import Bytes
import Bytes.Decode
import Bytes.Encode
import ConcurrentTask
import Json.Decode
import Json.Encode
import Maybe.Extra


port requests :
    ({ request : Json.Decode.Value
     , resolver : Json.Decode.Value
     }
     -> msg
    )
    -> Sub msg


port taskRequests : Json.Decode.Value -> Cmd msg


port taskResponses : (Json.Decode.Value -> msg) -> Sub msg


port errors : { context : String, error : Json.Encode.Value } -> Cmd msg


type Kv
    = Kv Json.Decode.Value


type AtomicOperation
    = AtomicOperation Json.Decode.Value


type Request
    = Request Json.Decode.Value


type Response
    = Response Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


type Versionstamp
    = Versionstamp String


type Error
    = JsException
        { function : String
        , message : String
        , raw : Json.Decode.Value
        }
    | ResponseDecoderFailure
        { function : String
        , error : Json.Decode.Error
        }
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


logError { context, error } =
    defineTask
        { function = "log:error"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "context", Json.Encode.string context )
                , ( "error", error )
                ]
        }


atomicOpCheck (AtomicOperation op) { key, versionstamp } =
    defineTask
        { function = "atomicOp:check"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "atomicOp", op )
                , ( "key", Json.Encode.list Json.Encode.string key )
                , ( "versionstamp"
                  , Maybe.Extra.unwrap Json.Encode.null
                        (\it ->
                            case it of
                                Versionstamp rawVersionstamp ->
                                    Json.Encode.string rawVersionstamp
                        )
                        versionstamp
                  )
                ]
        }


atomicOpCommit (AtomicOperation op) =
    defineTask
        { function = "atomicOp:commit"
        , expect =
            Json.Decode.field "ok" Json.Decode.bool
                |> Json.Decode.andThen
                    (\ok ->
                        if ok then
                            Json.Decode.map (Versionstamp >> Ok) (Json.Decode.field "versionstamp" Json.Decode.string)

                        else
                            Json.Decode.succeed (Err ())
                    )
                |> ConcurrentTask.expectJson
        , errors = ConcurrentTask.expectNoErrors
        , args = op
        }


atomicOpSet (AtomicOperation op) { key, value } =
    defineTask
        { function = "atomicOp:set"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "atomicOp", op )
                , ( "key", Json.Encode.list Json.Encode.string key )
                , ( "value", value )
                ]
        }


closeKv (Kv kv) =
    defineTask
        { function = "kv:close"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args = kv
        }


kvAtomic (Kv kv) =
    defineTask
        { function = "kv:atomic"
        , expect = ConcurrentTask.expectJson (Json.Decode.map AtomicOperation Json.Decode.value)
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
                    , Json.Decode.map2 (\versionstamp value -> { versionstamp = Versionstamp versionstamp, value = value })
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


getRandom bytes =
    defineTask
        { function = "random:get"
        , expect = ConcurrentTask.expectJson (Json.Decode.list Json.Decode.int)
        , errors = ConcurrentTask.expectNoErrors
        , args = Json.Encode.int bytes
        }


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
        , expect = ConcurrentTask.expectJson (Json.Decode.list Json.Decode.int)
        , errors = ConcurrentTask.expectNoErrors
        , args = request
        }
        |> ConcurrentTask.map (List.map Bytes.Encode.unsignedInt8 >> Bytes.Encode.sequence >> Bytes.Encode.encode)


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
                , ( "body"
                  , body
                        |> Bytes.Decode.decode
                            (Bytes.Decode.loop []
                                (\acc ->
                                    if List.length acc == Bytes.width body then
                                        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse acc))

                                    else
                                        Bytes.Decode.unsignedInt8
                                            |> Bytes.Decode.map (\byte -> Bytes.Decode.Loop (byte :: acc))
                                )
                            )
                        |> Maybe.withDefault []
                        |> Json.Encode.list Json.Encode.int
                  )
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
        |> ConcurrentTask.onJsException (\e -> ConcurrentTask.fail (JsException { function = definition.function, message = e.message, raw = e.raw }))
        |> ConcurrentTask.onResponseDecoderFailure (\e -> ConcurrentTask.fail (ResponseDecoderFailure { function = definition.function, error = e }))
