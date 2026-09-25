port module Backend.Interop exposing
    ( AtomicOperation
    , GlobalThis
    , Kv
    , KvKeyPart(..)
    , Log
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
    , getHeader
    , getLegacyResponse
    , getMethod
    , getRandom
    , getResponse
    , getUrl
    , globalThis
    , iteratorToList
    , kvAtomic
    , kvGet
    , kvList
    , logEmergency
    , logError
    , receiveRequests
    , receiveTaskProgress
    , resolveRequest
    , withKv
    )

import Bytes
import Bytes.Decode
import Bytes.Encode
import ConcurrentTask
import Ffi
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


type GlobalThis
    = GlobalThis Json.Encode.Value


type alias Log =
    List Json.Decode.Value


type Kv
    = Kv Json.Decode.Value


type KvKeyPart
    = StringKvKeyPart String
    | OpaqueKvKeyPart Json.Decode.Value


type AtomicOperation
    = AtomicOperation Json.Decode.Value


type Iterator a
    = Iterator { iterator : Json.Decode.Value, decoder : Json.Decode.Decoder a }


type Request
    = Request Json.Decode.Value


type Response
    = Response Json.Decode.Value


type Resolver
    = Resolver Json.Decode.Value


type Versionstamp
    = Versionstamp String


globalThis =
    GlobalThis


receiveRequests tag =
    requests
        (\{ request, resolver } -> tag { request = Request request, resolver = Resolver resolver })


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


getEnvironment globalThis_ key =
    deno globalThis_
        |> ConcurrentTask.andThen (\deno_ -> getProperty deno_ "env" Json.Decode.value)
        |> ConcurrentTask.andThen
            (\env ->
                callMethod env
                    "get"
                    [ Json.Encode.string key
                    ]
                    (Json.Decode.oneOf
                        [ Json.Decode.map Just Json.Decode.string
                        , Json.Decode.succeed Nothing
                        ]
                    )
            )


logError (GlobalThis globalThis_) errors =
    getProperty globalThis_ "console" Json.Decode.value
        |> ConcurrentTask.andThen
            (\console ->
                callMethod console "error" errors (Json.Decode.succeed {})
            )


logEmergency (GlobalThis globalThis_) errors =
    let
        decoder =
            Json.Decode.map2 (\console error -> { console = console, error = error })
                (Json.Decode.at [ "console" ] Json.Decode.value)
                (Json.Decode.at [ "console", "error" ] Json.Decode.value)
    in
    case Json.Decode.decodeValue decoder globalThis_ of
        Ok { console, error } ->
            Ffi.applyFunctionCmd error console errors taskRequests

        Err _ ->
            Cmd.none


atomicOpCheck (AtomicOperation op) { key, versionstamp } =
    callMethod op
        "check"
        [ Json.Encode.object
            [ ( "key", encodeKvKey key )
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
        ]
        (Json.Decode.succeed {})


atomicOpCommit (AtomicOperation op) =
    callMethod op
        "commit"
        []
        (Json.Decode.field "ok" Json.Decode.bool
            |> Json.Decode.andThen
                (\ok ->
                    if ok then
                        Json.Decode.map (Versionstamp >> Ok) (Json.Decode.field "versionstamp" Json.Decode.string)

                    else
                        Json.Decode.succeed (Err ())
                )
        )


atomicOpDelete (AtomicOperation op) { key } =
    callMethod op "delete" [ encodeKvKey key ] (Json.Decode.succeed {})


atomicOpSet (AtomicOperation op) { key, value } =
    callMethod op
        "set"
        [ encodeKvKey key
        , value
        ]
        (Json.Decode.succeed {})


closeKv (Kv kv) =
    callMethod kv "close" [] (Json.Decode.succeed {})


kvAtomic (Kv kv) =
    callMethod kv
        "atomic"
        []
        (Json.Decode.map AtomicOperation Json.Decode.value)


kvGet (Kv kv) key =
    callMethod kv
        "get"
        [ encodeKvKey key ]
        (Json.Decode.oneOf
            [ Json.Decode.field "versionstamp" (Json.Decode.null Nothing)
            , Json.Decode.map2 (\versionstamp value -> { versionstamp = Versionstamp versionstamp, value = value })
                (Json.Decode.field "versionstamp" Json.Decode.string)
                (Json.Decode.field "value" Json.Decode.value)
                |> Json.Decode.map Just
            ]
        )


kvList (Kv kv) prefix decoder =
    callMethod kv
        "list"
        [ Json.Encode.object
            [ ( "prefix", encodeKvKey prefix ) ]
        ]
        (Json.Decode.value
            |> Json.Decode.map
                (\iterator ->
                    Iterator
                        { iterator = iterator
                        , decoder =
                            Json.Decode.map3
                                (\key value versionstamp -> { key = key, value = value, versionstamp = versionstamp })
                                (Json.Decode.field "key" decodeKvKey)
                                (Json.Decode.field "value" Json.Decode.value)
                                (Json.Decode.map Versionstamp (Json.Decode.field "versionstamp" Json.Decode.string))
                        }
                )
        )


deno (GlobalThis globalThis_) =
    getProperty globalThis_ "Deno" Json.Decode.value


openKv globalThis_ =
    deno globalThis_
        |> ConcurrentTask.andThen
            (\deno_ ->
                callMethod deno_
                    "openKv"
                    []
                    (Json.Decode.map Kv Json.Decode.value)
            )


withKv globalThis_ task =
    openKv globalThis_
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


iteratorNext (Iterator { iterator, decoder }) =
    callMethod iterator
        "next"
        []
        (Json.Decode.field "done" Json.Decode.bool
            |> Json.Decode.andThen
                (\done ->
                    if done then
                        Json.Decode.succeed Nothing

                    else
                        Json.Decode.field "value" decoder
                            |> Json.Decode.map Just
                )
        )


iteratorToList iterator =
    iteratorNext iterator
        |> ConcurrentTask.andThen
            (\result ->
                case result of
                    Nothing ->
                        ConcurrentTask.succeed []

                    Just item ->
                        iteratorToList iterator
                            |> ConcurrentTask.map ((::) item)
            )


getRandom bytes =
    defineTask
        { function = "random:get"
        , expect = ConcurrentTask.expectJson (Json.Decode.list Json.Decode.int)
        , args = Json.Encode.int bytes
        }


getMethod (Request request) =
    getProperty request "method" Json.Decode.string


getUrl (Request request) =
    getProperty request "url" Json.Decode.string


getHeader (Request request) name =
    getProperty request "headers" Json.Decode.value
        |> ConcurrentTask.andThen
            (\headers ->
                callMethod headers
                    "get"
                    [ Json.Encode.string name ]
                    (Json.Decode.nullable Json.Decode.string)
            )


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
    Ffi.applyFunction resolver
        Json.Encode.null
        [ response ]
        (Json.Decode.succeed {})


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


decodeKvKey =
    Json.Decode.list
        (Json.Decode.oneOf
            [ Json.Decode.map StringKvKeyPart Json.Decode.string
            , Json.Decode.map OpaqueKvKeyPart Json.Decode.value
            ]
        )


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


getProperty request name decoder =
    Ffi.getProperty request (Json.Encode.string name) decoder


callMethod thisArg name argsArray decoder =
    getProperty thisArg name Json.Decode.value
        |> ConcurrentTask.andThen
            (\function ->
                Ffi.applyFunction function thisArg argsArray decoder
            )
