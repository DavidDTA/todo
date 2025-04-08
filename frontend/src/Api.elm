module Api exposing (Waypoint, WaypointId, priorities, waypointIdFromRaw, waypointIdToRaw)

import Http
import Json.Decode


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


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map waypointIdFromRaw Json.Decode.string))
