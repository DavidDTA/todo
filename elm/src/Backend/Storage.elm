module Backend.Storage exposing (getPriorities)

import Api
import Backend.Interop
import ConcurrentTask
import Json.Decode


getPriorities kv =
    Backend.Interop.kvGet kv [ "priorities" ] decodePriorities
        |> ConcurrentTask.map
            (\result ->
                case result of
                    Nothing ->
                        []

                    Just { value } ->
                        value
            )


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map Api.wrapWaypointId Json.Decode.string))
