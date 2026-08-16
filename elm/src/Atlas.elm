module Atlas exposing (WaypointId(..), waypointIdKeyDict)

import Graph
import KeyDict


type WaypointId
    = WaypointId String


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId id) -> id)
