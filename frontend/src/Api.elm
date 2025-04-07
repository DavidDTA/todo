module Api exposing (Waypoint, WaypointId, waypointIdFromRaw, waypointIdToRaw)


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
