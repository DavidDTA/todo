module Backend.Storage exposing
    ( Error(..)
    , addWaypoint
    , deleteWaypoint
    , getPriorities
    )

import Api
import Backend.Interop
import ConcurrentTask
import Json.Decode
import Json.Encode


type Error
    = KvValueDecodeError { key : List Backend.Interop.KvKeyPart, error : Json.Decode.Error }


getPriorities kv =
    kvGet kv keys.priorities decodePriorities
        |> ConcurrentTask.map (Maybe.withDefault [])


addWaypoint : Backend.Interop.AtomicOperation -> Api.WaypointId -> { text : String } -> ConcurrentTask.ConcurrentTask x ()
addWaypoint op id { text } =
    Backend.Interop.atomicOpCheck op { key = keys.waypoint id, versionstamp = Nothing }
        |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = keys.waypoint id, value = Json.Encode.object [ ( "text", Json.Encode.string text ) ] })


deleteWaypoint : Backend.Interop.AtomicOperation -> Api.WaypointId -> ConcurrentTask.ConcurrentTask x ()
deleteWaypoint op id =
    Backend.Interop.atomicOpDelete op { key = keys.waypoint id }


kvGet kv key decoder =
    Backend.Interop.kvGet kv key
        |> ConcurrentTask.andThen
            (\result ->
                case result of
                    Nothing ->
                        ConcurrentTask.succeed Nothing

                    Just { value } ->
                        case Json.Decode.decodeValue decoder value of
                            Ok decoded ->
                                ConcurrentTask.succeed (Just decoded)

                            Err err ->
                                ConcurrentTask.fail (KvValueDecodeError { key = key, error = err })
            )


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map Api.wrapWaypointId Json.Decode.string))


keys =
    { priorities = [ Backend.Interop.StringKvKeyPart "priorities" ]
    , waypoint =
        \id ->
            [ Backend.Interop.StringKvKeyPart "waypoints"
            , Backend.Interop.StringKvKeyPart (Api.unwrapWaypointId id)
            ]
    }
