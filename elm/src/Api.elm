module Api exposing
    ( Waypoint
    , WaypointId(..)
    , addWaypoint
    , appjs
    , badRequest
    , detail
    , forbidden
    , home
    , internalServerError
    , login
    , priorities
    , unwrapWaypointId
    , waypointIdKeyDict
    , waypoints
    , wrapWaypointId
    )

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
import Url


type WaypointId
    = WaypointId String


type alias Waypoint =
    { text : String
    , completed : Bool
    , url : Maybe String
    , requires : List WaypointId
    , requiredBy : List WaypointId
    }


wrapWaypointId =
    WaypointId


unwrapWaypointId (WaypointId s) =
    s


home =
    get [ Endpoint.fixed "" ]
        opaqueResponse


detail =
    get [ Endpoint.fixed "detail", Endpoint.wildcard ]
        opaqueResponse


appjs =
    get [ Endpoint.fixed "-", Endpoint.fixed "app.js" ]
        opaqueResponse


apiBase =
    [ Endpoint.fixed "-", Endpoint.fixed "api" ]


login =
    post
        (apiBase ++ [ Endpoint.fixed "login" ])
        opaqueRequest
        opaqueResponse


priorities :
    Endpoint.Endpoint
        ((Result Http.Error (List WaypointId) -> msg) -> Cmd msg)
        (ConcurrentTask.ConcurrentTask Backend.Interop.Error (List WaypointId)
         -> Backend.Interop.Request
         -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        )
priorities =
    get
        (apiBase ++ [ Endpoint.fixed "priorities" ])
        (bytesResponse w3_decode_Priorities w3_encode_Priorities)


waypoints =
    get
        (apiBase ++ [ Endpoint.fixed "waypoints" ])
        { expectResponse = (jsonResponse decodeWaypoints (always Json.Encode.null)).expectResponse
        , handleResult = opaqueResponse.handleResult
        }


addWaypoint =
    post
        (apiBase ++ [ Endpoint.fixed "waypoints" ])
        (bytesRequest w3_decode_AddWaypointRequest w3_encode_AddWaypointRequest)
        (bytesResponse w3_decode_AddWaypointResponse w3_encode_AddWaypointResponse)


type alias Priorities =
    List WaypointId


decodeWaypoint =
    Json.Decode.map6
        (\id text completed url requires requiredBy -> { id = WaypointId id, waypoint = Waypoint text completed url requires requiredBy })
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "completed" Json.Decode.bool)
        (Json.Decode.field "url" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "requires" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string)))
        (Json.Decode.field "requiredBy" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string)))


decodeWaypoints =
    Json.Decode.field
        "waypoints"
        (decodeWaypoint
            |> Json.Decode.map (\{ id, waypoint } -> ( id, waypoint ))
            |> Json.Decode.list
            |> Json.Decode.andThen
                (\list ->
                    let
                        dict =
                            waypointIdKeyDict .fromList list
                    in
                    if waypointIdKeyDict .size dict == List.length list then
                        Json.Decode.succeed dict

                    else
                        Json.Decode.fail "Duplicate id"
                )
        )


type alias AddWaypointRequest =
    { text : String }


type alias AddWaypointResponse =
    { id : WaypointId, waypoint : Waypoint }


emptyRequest =
    { applyBody = \fn method path -> fn method path Http.emptyBody
    , handleBody =
        \task request continue ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        if Bytes.width body == 0 then
                            continue task request

                        else
                            badRequest
                    )
    }


opaqueRequest =
    { applyBody = identity
    , handleBody = \task request handleResult -> handleResult (task request) request
    }


bytesRequest :
    Bytes.Decode.Decoder r
    -> (r -> Lamdera.Wire3.Encoder)
    ->
        { applyBody :
            (String -> List String -> Http.Body -> (Result Http.Error t -> msg) -> Cmd msg)
            -> String
            -> List String
            -> r
            -> (Result Http.Error t -> msg)
            -> Cmd msg
        , handleBody :
            (r -> impl2)
            -> Backend.Interop.Request
            -> (impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
bytesRequest decoder encoder =
    { applyBody = \fn method path value -> fn method path (Http.bytesBody "application/octet-stream" (Bytes.Encode.encode (encoder value)))
    , handleBody =
        \task request handleResult ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        case Bytes.Decode.decode decoder body of
                            Just value ->
                                handleResult (task value) request

                            Nothing ->
                                badRequest
                    )
    }


bytesResponse :
    Bytes.Decode.Decoder t
    -> (t -> Bytes.Encode.Encoder)
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult :
            ConcurrentTask.ConcurrentTask Backend.Interop.Error t
            -> Backend.Interop.Request
            -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
bytesResponse decoder encoder =
    { expectResponse = \tag -> Http.expectBytes tag decoder
    , handleResult =
        \impl _ ->
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
    Json.Decode.Decoder t
    -> (t -> Json.Encode.Value)
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult :
            ConcurrentTask.ConcurrentTask Backend.Interop.Error t
            -> Backend.Interop.Request
            -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
jsonResponse decoder encoder =
    { expectResponse = \tag -> Http.expectJson tag decoder
    , handleResult =
        \impl _ ->
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
    , handleResult = \impl req -> impl req
    }


get :
    List Endpoint.PathComponent
    ->
        { expectResponse : (Result Http.Error response -> msg) -> Http.Expect msg
        , handleResult : impl -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            ((Result Http.Error response -> msg) -> Cmd msg)
            (impl -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response)
get path response =
    Endpoint.endpoint "GET" path (endpoint emptyRequest response)


post :
    List Endpoint.PathComponent
    ->
        { applyBody :
            (String -> List String -> Http.Body -> (Result Http.Error t -> msg) -> Cmd msg)
            -> String
            -> List String
            -> r
            -> (Result Http.Error t -> msg)
            -> Cmd msg
        , handleBody :
            (r1 -> impl2)
            -> Backend.Interop.Request
            -> (impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            (r -> (Result Http.Error t -> msg) -> Cmd msg)
            ((r1 -> impl2) -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response)
post path request response =
    Endpoint.endpoint "POST" path (endpoint request response)


endpoint :
    { applyBody :
        (String
         -> List String
         -> Http.Body
         -> (Result Http.Error t -> msg)
         -> Cmd msg
        )
        -> req
    , handleBody :
        impl1
        -> Backend.Interop.Request
        -> (impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response)
        -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
    }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
    ->
        { request : req
        , response : impl1 -> Backend.Interop.Request -> ConcurrentTask.ConcurrentTask Backend.Interop.Error Backend.Interop.Response
        }
endpoint { applyBody, handleBody } { expectResponse, handleResult } =
    { request =
        applyBody
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
            handleBody task request handleResult
    }


badRequest =
    Backend.Interop.getResponse { status = 400, body = emptyBody }


forbidden =
    Backend.Interop.getResponse { status = 403, body = emptyBody }


internalServerError =
    Backend.Interop.getResponse { status = 500, body = emptyBody }


emptyBody =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId id) -> id)
