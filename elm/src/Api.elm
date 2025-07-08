module Api exposing
    ( Waypoint
    , WaypointId
    , badRequest
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
import ConcurrentTask
import Endpoint
import Http
import Json.Decode
import Json.Encode
import KeyDict
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
    get [ "" ]
        opaqueResponse


apiBase =
    [ "-", "api" ]


login =
    post
        (apiBase ++ [ "login" ])
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
        (apiBase ++ [ "priorities" ])
        (jsonResponse decodePriorities encodePriorities)


waypoints =
    get
        (apiBase ++ [ "waypoints" ])
        { expectResponse = (jsonResponse decodeWaypoints (always Json.Encode.null)).expectResponse
        , handleResult = opaqueResponse.handleResult
        }


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string))


encodePriorities priorities_ =
    Json.Encode.object
        [ ( "priorities"
          , Json.Encode.list (\(WaypointId id) -> Json.Encode.string id) priorities_
          )
        ]


decodeWaypoints =
    Json.Decode.field
        "waypoints"
        (Json.Decode.list
            (Json.Decode.map6
                (\id text completed url requires requiredBy -> ( WaypointId id, Waypoint text completed url requires requiredBy ))
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "text" Json.Decode.string)
                (Json.Decode.field "completed" Json.Decode.bool)
                (Json.Decode.field "url" (Json.Decode.nullable Json.Decode.string))
                (Json.Decode.field "requires" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string)))
                (Json.Decode.field "requiredBy" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string)))
            )
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


emptyRequest =
    { applyBody = \fn method path -> fn method path Http.emptyBody
    , handleBody =
        \task request continue ->
            Backend.Interop.getBody request
                |> ConcurrentTask.andThen
                    (\body ->
                        case body of
                            "" ->
                                continue task request

                            _ ->
                                badRequest
                    )
    }


opaqueRequest =
    { applyBody = identity
    , handleBody = \task request handleResult -> handleResult (task request) request
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
                            , body = Json.Encode.encode 0 (encoder value)
                            }
                    )
    }


opaqueResponse =
    { expectResponse = Http.expectWhatever
    , handleResult = \impl req -> impl req
    }


get :
    List String
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
    List String
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
    Backend.Interop.getResponse { status = 400, body = "" }


forbidden =
    Backend.Interop.getResponse { status = 403, body = "" }


internalServerError =
    Backend.Interop.getResponse { status = 500, body = "" }


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId id) -> id)
