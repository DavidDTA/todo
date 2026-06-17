module Api exposing
    ( Waypoint
    , WaypointId(..)
    , addWaypoint
    , appjs
    , badRequest
    , deleteWaypoint
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
        (ConcurrentTask.ConcurrentTask x (List WaypointId)
         -> Backend.Interop.Request
         -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
         -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
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
        (apiBase ++ [ Endpoint.fixed "add-waypoint" ])
        (bytesRequest w3_decode_AddWaypointRequest w3_encode_AddWaypointRequest)
        (bytesResponse w3_decode_AddWaypointResponse w3_encode_AddWaypointResponse)


deleteWaypoint =
    post
        (apiBase ++ [ Endpoint.fixed "delete-waypoint" ])
        (bytesRequest w3_decode_DeleteWaypointRequest w3_encode_DeleteWaypointRequest)
        (bytesResponse w3_decode_DeleteWaypointResponse w3_encode_DeleteWaypointResponse)


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


type alias DeleteWaypointRequest =
    { id : WaypointId }


type alias DeleteWaypointResponse =
    {}


emptyRequest =
    { applyBody = \fn method path -> fn method path Http.emptyBody
    , handleBody =
        \task request handleErrors continue ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        if Bytes.width body == 0 then
                            continue task request handleErrors

                        else
                            badRequest
                    )
    }


opaqueRequest =
    { applyBody = identity
    , handleBody = \task request handleErrors handleResult -> handleResult (task request) request handleErrors
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
            (r -> ConcurrentTask.ConcurrentTask x t)
            -> Backend.Interop.Request
            -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> (ConcurrentTask.ConcurrentTask x t -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
bytesRequest decoder encoder =
    { applyBody = \fn method path value -> fn method path (Http.bytesBody "application/octet-stream" (Bytes.Encode.encode (encoder value)))
    , handleBody =
        \task request handleErrors handleResult ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        case Bytes.Decode.decode decoder body of
                            Just value ->
                                handleResult (task value) request handleErrors

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
            ConcurrentTask.ConcurrentTask x t
            -> Backend.Interop.Request
            -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
bytesResponse decoder encoder =
    { expectResponse = \tag -> Http.expectBytes tag decoder
    , handleResult =
        \impl _ errorHandler ->
            impl
                |> ConcurrentTask.map Result.Ok
                |> ConcurrentTask.onError (Result.Err >> ConcurrentTask.succeed)
                |> ConcurrentTask.andThen
                    (\result ->
                        case result of
                            Ok value ->
                                Backend.Interop.getResponse
                                    { status = 200
                                    , body =
                                        Bytes.Encode.encode (encoder value)
                                    }

                            Err error ->
                                errorHandler error
                    )
    }


jsonResponse :
    Json.Decode.Decoder t
    -> (t -> Json.Encode.Value)
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult :
            ConcurrentTask.ConcurrentTask x t
            -> Backend.Interop.Request
            -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
jsonResponse decoder encoder =
    { expectResponse = \tag -> Http.expectJson tag decoder
    , handleResult =
        \impl _ handleErrors ->
            impl
                |> ConcurrentTask.map Result.Ok
                |> ConcurrentTask.onError (Result.Err >> ConcurrentTask.succeed)
                |> ConcurrentTask.andThen
                    (\result ->
                        case result of
                            Ok value ->
                                Backend.Interop.getResponse
                                    { status = 200
                                    , body =
                                        Json.Encode.encode 0 (encoder value)
                                            |> Bytes.Encode.string
                                            |> Bytes.Encode.encode
                                    }

                            Err error ->
                                handleErrors error
                    )
    }


opaqueResponse =
    { expectResponse = Http.expectWhatever
    , handleResult = \impl req handleErrors -> impl req
    }


get :
    List Endpoint.PathComponent
    ->
        { expectResponse : (Result Http.Error response -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            ((Result Http.Error response -> msg) -> Cmd msg)
            (impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
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
            impl
            -> Backend.Interop.Request
            -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> (impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
            -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            (r -> (Result Http.Error t -> msg) -> Cmd msg)
            (impl -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
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
        impl
        -> Backend.Interop.Request
        -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
        -> (impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
        -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
    }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        { request : req
        , response : impl -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
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
        \task request handleErrors ->
            handleBody task request handleErrors handleResult
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
