module Backend.Storage exposing
    ( addWaypoint
    , deleteWaypoint
    , getPriorities
    )

import Api
import Backend.Interop
import ConcurrentTask
import Json.Decode
import Json.Encode


getPriorities kv =
    Backend.Interop.kvGet kv keys.priorities decodePriorities
        |> ConcurrentTask.map
            (\result ->
                case result of
                    Nothing ->
                        []

                    Just { value } ->
                        value
            )


addWaypoint : Backend.Interop.AtomicOperation -> Api.WaypointId -> { text : String } -> ConcurrentTask.ConcurrentTask Backend.Interop.Error ()
addWaypoint op id { text } =
    Backend.Interop.atomicOpCheck op { key = keys.waypoint id, versionstamp = Nothing }
        |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = keys.waypoint id, value = Json.Encode.object [ ( "text", Json.Encode.string text ) ] })


deleteWaypoint : Backend.Interop.AtomicOperation -> Api.WaypointId -> ConcurrentTask.ConcurrentTask Backend.Interop.Error ()
deleteWaypoint op id =
    Backend.Interop.atomicOpDelete op { key = keys.waypoint id }


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map Api.wrapWaypointId Json.Decode.string))


keys =
    { priorities = [ "priorities" ]
    , waypoint = \id -> [ "waypoints", Api.unwrapWaypointId id ]
    }
