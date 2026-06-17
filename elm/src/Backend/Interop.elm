port module Backend.Interop exposing
    ( AtomicOperation
    , Kv
    , KvKeyPart(..)
    , Request
    , Resolver
    , Response
    , atomicOpCheck
    , atomicOpCommit
    , atomicOpDelete
    , atomicOpSet
    , attemptTask
    , encodeKvKey
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


port errors : List Json.Encode.Value -> Cmd msg


type Kv
    = Kv Json.Decode.Value


type KvKeyPart
    = StringKvKeyPart String
    | OpaqueKvKeyPart Json.Decode.Value


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
        , args = Json.Encode.string key
        }


logError errors_ =
    defineTask
        { function = "log:error"
        , expect = ConcurrentTask.expectWhatever
        , args = Json.Encode.list identity errors_
        }


atomicOpCheck (AtomicOperation op) { key, versionstamp } =
    defineTask
        { function = "atomicOp:check"
        , expect = ConcurrentTask.expectWhatever
        , args =
            Json.Encode.object
                [ ( "atomicOp", op )
                , ( "key", encodeKvKey key )
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
        , args = op
        }


atomicOpDelete (AtomicOperation op) { key } =
    defineTask
        { function = "atomicOp:delete"
        , expect = ConcurrentTask.expectWhatever
        , args =
            Json.Encode.object
                [ ( "atomicOp", op )
                , ( "key", encodeKvKey key )
                ]
        }


atomicOpSet (AtomicOperation op) { key, value } =
    defineTask
        { function = "atomicOp:set"
        , expect = ConcurrentTask.expectWhatever
        , args =
            Json.Encode.object
                [ ( "atomicOp", op )
                , ( "key", encodeKvKey key )
                , ( "value", value )
                ]
        }


closeKv (Kv kv) =
    defineTask
        { function = "kv:close"
        , expect = ConcurrentTask.expectWhatever
        , args = kv
        }


kvAtomic (Kv kv) =
    defineTask
        { function = "kv:atomic"
        , expect = ConcurrentTask.expectJson (Json.Decode.map AtomicOperation Json.Decode.value)
        , args = kv
        }


kvGet (Kv kv) key =
    defineTask
        { function = "kv:get"
        , expect =
            ConcurrentTask.expectJson
                (Json.Decode.oneOf
                    [ Json.Decode.field "versionstamp" (Json.Decode.null Nothing)
                    , Json.Decode.map2 (\versionstamp value -> { versionstamp = Versionstamp versionstamp, value = value })
                        (Json.Decode.field "versionstamp" Json.Decode.string)
                        (Json.Decode.field "value" Json.Decode.value)
                        |> Json.Decode.map Just
                    ]
                )
        , args =
            Json.Encode.object
                [ ( "kv", kv )
                , ( "key", encodeKvKey key )
                ]
        }


openKv =
    defineTask
        { function = "kv:open"
        , expect = ConcurrentTask.expectJson (Json.Decode.map Kv Json.Decode.value)
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
        , args = Json.Encode.int bytes
        }


getMethod (Request request) =
    defineTask
        { function = "req:getMethod"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , args = request
        }


getUrl (Request request) =
    defineTask
        { function = "req:getUrl"
        , expect = ConcurrentTask.expectJson Json.Decode.string
        , args = request
        }


getCookie key (Request request) =
    defineTask
        { function = "req:getCookie"
        , expect = ConcurrentTask.expectJson (Json.Decode.nullable Json.Decode.string)
        , args =
            Json.Encode.object
                [ ( "request", request ), ( "key", Json.Encode.string key ) ]
        }


getBody (Request request) =
    defineTask
        { function = "req:getBody"
        , expect = ConcurrentTask.expectJson (Json.Decode.list Json.Decode.int)
        , args = request
        }
        |> ConcurrentTask.map (List.map Bytes.Encode.unsignedInt8 >> Bytes.Encode.sequence >> Bytes.Encode.encode)


resolveRequest (Resolver resolver) (Response response) =
    defineTask
        { function = "req:resolve"
        , expect = ConcurrentTask.expectWhatever
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
        , args = request
        }


encodeKvKey key =
    Json.Encode.list
        (\part ->
            case part of
                StringKvKeyPart s ->
                    Json.Encode.string s

                OpaqueKvKeyPart o ->
                    o
        )
        key


defineTask :
    { function : String
    , expect : ConcurrentTask.Expect a
    , args : Json.Decode.Value
    }
    -> ConcurrentTask.ConcurrentTask x a
defineTask { function, expect, args } =
    ConcurrentTask.define
        { function = function
        , expect = expect
        , errors = ConcurrentTask.expectNoErrors
        , args = args
        }
