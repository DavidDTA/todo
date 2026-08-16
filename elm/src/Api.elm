module Api exposing
    ( Waypoint
    , Waypoints
    , appjs
    , badRequest
    , detail
    , export
    , forbidden
    , home
    , internalServerError
    , login
    , unwrapWaypointId
    , waypointAdd
    , waypointDelete
    , waypointSetCompleted
    , waypointSetPriority
    , waypoints
    , wrapWaypointId
    )

import Atlas
import Backend.Interop
import Bytes
import Bytes.Decode
import Bytes.Encode
import ConcurrentTask
import Endpoint
import Http
import Json.Decode
import Json.Encode
import KeyDict
import Lamdera.Wire3
import Maybe.Extra
import SortKey
import Url


type alias Waypoint =
    { text : String
    , completed : Bool
    , priority : Maybe String
    , url : Maybe String
    , requires : List Atlas.WaypointId
    , requiredBy : List Atlas.WaypointId
    }


type alias Waypoints =
    List { id : Atlas.WaypointId, waypoint : Waypoint }


wrapWaypointId =
    Atlas.WaypointId


unwrapWaypointId (Atlas.WaypointId s) =
    s


home =
    get [ Endpoint.fixed "" ]
        opaqueResponse
        |> withRequest


detail =
    get [ Endpoint.fixed "detail", Endpoint.wildcard ]
        opaqueResponse
        |> withRequest


export =
    get [ Endpoint.fixed "account", Endpoint.fixed "export" ]
        (jsonResponse
            (\response ->
                Json.Encode.object
                    [ ( "waypoints"
                      , response.waypoints
                            |> List.map
                                (\{ id, waypoint } ->
                                    ( unwrapWaypointId id
                                    , Json.Encode.object
                                        [ ( "text", Json.Encode.string waypoint.text )
                                        , ( "completed", Json.Encode.bool waypoint.completed )
                                        , ( "priority", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string waypoint.priority )
                                        , ( "url", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string waypoint.url )
                                        , ( "requires", Json.Encode.list (unwrapWaypointId >> Json.Encode.string) waypoint.requires )
                                        , ( "requiredBy", Json.Encode.list (unwrapWaypointId >> Json.Encode.string) waypoint.requiredBy )
                                        ]
                                    )
                                )
                            |> Json.Encode.object
                      )
                    ]
            )
        )


appjs =
    get [ Endpoint.fixed "-", Endpoint.fixed "app.js" ]
        opaqueResponse
        |> withRequest


apiBase =
    [ Endpoint.fixed "-", Endpoint.fixed "api" ]


login =
    post
        (apiBase ++ [ Endpoint.fixed "login" ])
        opaqueRequest
        opaqueResponse


waypoints =
    get
        (apiBase ++ [ Endpoint.fixed "waypoints" ])
        (bytesResponse w3_decode_Waypoints w3_encode_Waypoints)


waypointAdd =
    post
        (apiBase ++ [ Endpoint.fixed "add-waypoint" ])
        (bytesRequest w3_decode_WaypointAddRequest w3_encode_WaypointAddRequest)
        (bytesResponse w3_decode_WaypointAddResponse w3_encode_WaypointAddResponse)


waypointDelete =
    post
        (apiBase ++ [ Endpoint.fixed "delete-waypoint" ])
        (bytesRequest w3_decode_WaypointDeleteRequest w3_encode_WaypointDeleteRequest)
        emptyResponse


waypointSetCompleted =
    post
        (apiBase ++ [ Endpoint.fixed "waypoint-set-completed" ])
        (bytesRequest w3_decode_WaypointSetCompletedRequest w3_encode_WaypointSetCompletedRequest)
        emptyResponse


waypointSetPriority =
    post
        (apiBase ++ [ Endpoint.fixed "waypoint-set-priority" ])
        (bytesRequest w3_decode_WaypointSetPriorityRequest w3_encode_WaypointSetPriorityRequest)
        emptyResponse


type alias WaypointAddRequest =
    { text : String }


type alias WaypointAddResponse =
    { id : Atlas.WaypointId, waypoint : Waypoint }


type alias WaypointDeleteRequest =
    { id : Atlas.WaypointId }


type alias WaypointSetCompletedRequest =
    { id : Atlas.WaypointId
    , completed : Bool
    }


type alias WaypointSetPriorityRequest =
    { id : Atlas.WaypointId
    , priority : Maybe String
    }


withRequest =
    Endpoint.mapResponse (\handler impl request -> handler (impl request) request)


emptyRequest :
    { encodeRequest :
        (String -> List String -> Http.Body -> (Result Http.Error t -> msg) -> Cmd msg)
        -> String
        -> List String
        -> (Result Http.Error t -> msg)
        -> Cmd msg
    , decodeRequest :
        Backend.Interop.Request -> ConcurrentTask.ConcurrentTask () {}
    , applyRequest : {} -> impl -> impl
    }
emptyRequest =
    { encodeRequest = \fn method path -> fn method path Http.emptyBody
    , decodeRequest =
        \request ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        if Bytes.width body == 0 then
                            ConcurrentTask.succeed {}

                        else
                            ConcurrentTask.fail ()
                    )
    , applyRequest = always identity
    }


emptyResponse :
    { expectResponse : (Result Http.Error {} -> msg) -> Http.Expect msg
    , handleResult :
        ConcurrentTask.ConcurrentTask x t
        -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
    }
emptyResponse =
    { expectResponse = \tag -> Http.expectWhatever (Result.map (\() -> {}) >> tag)
    , handleResult =
        \impl ->
            impl
                |> ConcurrentTask.andThenDo
                    (Backend.Interop.getResponse
                        { status = 200
                        , body = Bytes.Encode.encode (Bytes.Encode.sequence [])
                        }
                    )
    }


opaqueRequest =
    { encodeRequest = identity
    , decodeRequest = ConcurrentTask.succeed
    , applyRequest = \r task -> task r
    }


bytesRequest :
    Bytes.Decode.Decoder r
    -> (r -> Lamdera.Wire3.Encoder)
    ->
        { encodeRequest :
            (String -> List String -> Http.Body -> (Result Http.Error t -> msg) -> Cmd msg)
            -> String
            -> List String
            -> r
            -> (Result Http.Error t -> msg)
            -> Cmd msg
        , decodeRequest :
            Backend.Interop.Request -> ConcurrentTask.ConcurrentTask () r
        , applyRequest : r -> (r -> impl) -> impl
        }
bytesRequest decoder encoder =
    { encodeRequest = \fn method path value -> fn method path (Http.bytesBody "application/octet-stream" (Bytes.Encode.encode (encoder value)))
    , decodeRequest =
        \request ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        case Bytes.Decode.decode decoder body of
                            Just value ->
                                ConcurrentTask.succeed value

                            Nothing ->
                                ConcurrentTask.fail ()
                    )
    , applyRequest = \r task -> task r
    }


bytesResponse :
    Bytes.Decode.Decoder t
    -> (t -> Bytes.Encode.Encoder)
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult :
            ConcurrentTask.ConcurrentTask x t
            -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
bytesResponse decoder encoder =
    { expectResponse = \tag -> Http.expectBytes tag decoder
    , handleResult =
        \impl ->
            impl
                |> ConcurrentTask.andThen
                    (\value ->
                        Backend.Interop.getResponse
                            { status = 200
                            , body =
                                Bytes.Encode.encode (encoder value)
                            }
                    )
    }


jsonResponse :
    (t -> Json.Encode.Value)
    ->
        { expectResponse : (Result Http.Error Json.Encode.Value -> msg) -> Http.Expect msg
        , handleResult :
            ConcurrentTask.ConcurrentTask x t
            -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
jsonResponse encoder =
    { expectResponse = \tag -> Http.expectJson tag Json.Decode.value
    , handleResult =
        \impl ->
            impl
                |> ConcurrentTask.andThen
                    (\value ->
                        Backend.Interop.getResponse
                            { status = 200
                            , body =
                                Json.Encode.encode 0 (encoder value)
                                    |> Bytes.Encode.string
                                    |> Bytes.Encode.encode
                            }
                    )
    }


opaqueResponse =
    { expectResponse = Http.expectWhatever
    , handleResult = identity
    }


get :
    List Endpoint.PathComponent
    ->
        { expectResponse : (Result Http.Error response -> msg) -> Http.Expect msg
        , handleResult : impl2 -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            ((Result Http.Error response -> msg) -> Cmd msg)
            (impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response)
get path response =
    Endpoint.endpoint "GET" path (endpoint emptyRequest response)


post :
    List Endpoint.PathComponent
    ->
        { encodeRequest :
            (String -> List String -> Http.Body -> (Result Http.Error t -> msg) -> Cmd msg)
            -> String
            -> List String
            -> req
        , decodeRequest :
            Backend.Interop.Request -> ConcurrentTask.ConcurrentTask xr r2
        , applyRequest : r2 -> impl2 -> impl3
        }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl3 -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            req
            (impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response)
post path request response =
    Endpoint.endpoint "POST" path (endpoint request response)


endpoint :
    { encodeRequest :
        (String
         -> List String
         -> Http.Body
         -> (Result Http.Error t -> msg)
         -> Cmd msg
        )
        -> req
    , decodeRequest :
        Backend.Interop.Request -> ConcurrentTask.ConcurrentTask xr r2
    , applyRequest : r2 -> impl2 -> impl3
    }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl3 -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
    ->
        { request : req
        , response : impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask x Backend.Interop.Response
        }
endpoint { encodeRequest, decodeRequest, applyRequest } { expectResponse, handleResult } =
    { request =
        encodeRequest
            (\method pathSegments body tag ->
                Http.request
                    { method = method
                    , headers = []
                    , url = "/" ++ String.join "/" (List.map Url.percentEncode pathSegments)
                    , body = body
                    , expect = expectResponse tag
                    , timeout = Nothing
                    , tracker = Nothing
                    }
            )
    , response =
        \task request ->
            decodeRequest request
                |> ConcurrentTask.map Ok
                |> ConcurrentTask.mapError Err
                |> ConcurrentTask.onError ConcurrentTask.succeed
                |> ConcurrentTask.andThen
                    (\r ->
                        case r of
                            Ok value ->
                                handleResult (applyRequest value task)

                            Err _ ->
                                badRequest
                    )
    }


badRequest =
    Backend.Interop.getResponse { status = 400, body = emptyBody }


forbidden =
    Backend.Interop.getResponse { status = 403, body = emptyBody }


internalServerError =
    Backend.Interop.getResponse { status = 500, body = emptyBody }


emptyBody =
    Bytes.Encode.encode (Bytes.Encode.sequence [])
