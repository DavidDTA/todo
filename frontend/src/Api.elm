module Api exposing (Waypoint, WaypointId, priorities, waypointIdKeyDict, waypoints)

import Http
import Json.Decode
import KeyDict


type WaypointId
    = WaypointId String


type alias Waypoint =
    { text : String
    , completed : Bool
    , url : Maybe String
    , requires : List WaypointId
    , requiredBy : List WaypointId
    }


waypointIdFromRaw =
    WaypointId


waypointIdToRaw (WaypointId raw) =
    raw


priorities tag =
    Http.get
        { url = "/-/api/priorities"
        , expect = Http.expectJson tag decodePriorities
        }


waypoints tag =
    Http.get
        { url = "/-/api/waypoints"
        , expect = Http.expectJson tag decodeWaypoints
        }


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map waypointIdFromRaw Json.Decode.string))


decodeWaypoints =
    Json.Decode.field
        "waypoints"
        (Json.Decode.list
            (Json.Decode.map6
                (\id text completed url requires requiredBy -> ( waypointIdFromRaw id, Waypoint text completed url requires requiredBy ))
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "text" Json.Decode.string)
                (Json.Decode.field "completed" Json.Decode.bool)
                (Json.Decode.field "url" (Json.Decode.nullable Json.Decode.string))
                (Json.Decode.field "requires" (Json.Decode.list (Json.Decode.map waypointIdFromRaw Json.Decode.string)))
                (Json.Decode.field "requiredBy" (Json.Decode.list (Json.Decode.map waypointIdFromRaw Json.Decode.string)))
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


waypointIdKeyDict =
    KeyDict.define waypointIdFromRaw waypointIdToRaw
