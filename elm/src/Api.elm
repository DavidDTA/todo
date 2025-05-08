module Api exposing
    ( Waypoint
    , WaypointId
    , home
    , login
    , priorities
    , waypointIdKeyDict
    , waypoints
    )

import Endpoint
import Http
import Json.Decode
import KeyDict
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


home =
    Endpoint.get [ "" ] (\_ _ -> never) identity


apiBase =
    [ "-", "api" ]


login =
    Endpoint.post
        (apiBase ++ [ "login" ])
        (\_ _ -> never)
        identity


priorities =
    Endpoint.get
        (apiBase ++ [ "priorities" ])
        (jsonRequest decodePriorities)
        never


waypoints =
    Endpoint.get
        (apiBase ++ [ "waypoints" ])
        (jsonRequest decodeWaypoints)
        never


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map WaypointId Json.Decode.string))


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


jsonRequest decoder method pathSegments tag =
    Http.request
        { method = method
        , headers = []
        , url = "/" ++ String.join "/" (List.map Url.percentEncode pathSegments)
        , body = Http.emptyBody
        , expect = Http.expectJson tag decoder
        , timeout = Nothing
        , tracker = Nothing
        }


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId id) -> id)
