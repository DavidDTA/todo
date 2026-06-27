module Api exposing
    ( Waypoint
    , WaypointId(..)
    , Waypoints
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


type alias Waypoints =
    List { id : WaypointId, waypoint : Waypoint }


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
        (bytesResponse w3_decode_Waypoints w3_encode_Waypoints)


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
        \request ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        if Bytes.width body == 0 then
                            ConcurrentTask.succeed ()

                        else
                            ConcurrentTask.fail ()
                    )
    }


opaqueRequest =
    { applyBody = identity
    , handleBody = ConcurrentTask.succeed
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
            Backend.Interop.Request -> ConcurrentTask.ConcurrentTask () r
        }
bytesRequest decoder encoder =
    { applyBody = \fn method path value -> fn method path (Http.bytesBody "application/octet-stream" (Bytes.Encode.encode (encoder value)))
    , handleBody =
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
        |> Endpoint.mapResponse (\responder impl -> responder (always impl))


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
            Backend.Interop.Request -> ConcurrentTask.ConcurrentTask xr r2
        }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        Endpoint.Endpoint
            (r -> (Result Http.Error t -> msg) -> Cmd msg)
            ((r2 -> impl2) -> Backend.Interop.Request -> (x -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response)
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
        Backend.Interop.Request -> ConcurrentTask.ConcurrentTask xr r2
    }
    ->
        { expectResponse : (Result Http.Error t -> msg) -> Http.Expect msg
        , handleResult : impl2 -> Backend.Interop.Request -> (x2 -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
        }
    ->
        { request : req
        , response : (r2 -> impl2) -> Backend.Interop.Request -> (x2 -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response) -> ConcurrentTask.ConcurrentTask Never Backend.Interop.Response
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
            handleBody request
                |> ConcurrentTask.map Ok
                |> ConcurrentTask.mapError Err
                |> ConcurrentTask.onError ConcurrentTask.succeed
                |> ConcurrentTask.andThen
                    (\r ->
                        case r of
                            Ok value ->
                                handleResult (task value) request handleErrors

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


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId id) -> id)
