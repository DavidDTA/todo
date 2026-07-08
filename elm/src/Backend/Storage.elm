module Backend.Storage exposing
    ( PrioritiesListError(..)
    , WaypointSetCompletedError(..)
    , WaypointsListError(..)
    , prioritiesList
    , waypointAdd
    , waypointDelete
    , waypointSetCompleted
    , waypointsList
    )

import Api
import Backend.Interop
import ConcurrentTask
import Errors
import Json.Decode
import Json.Encode
import Result.Extra


type WaypointsListError
    = WaypointsListUnexpectedKey (List Backend.Interop.KvKeyPart)
    | WaypointsListDecodeError { id : Api.WaypointId, error : Json.Decode.Error }


type PrioritiesListError
    = PrioritiesListDecodeError Json.Decode.Error


type WaypointSetCompletedError
    = WaypointSetCompletedMissingWaypoint
    | WaypointSetCompletedDecodeError Json.Decode.Error


prioritiesList kv =
    kvGet kv keys.priorities decodePriorities PrioritiesListDecodeError
        |> ConcurrentTask.map (Maybe.map .value >> Maybe.withDefault [])


waypointAdd : Backend.Interop.AtomicOperation -> Api.WaypointId -> { text : String } -> ConcurrentTask.ConcurrentTask x ()
waypointAdd op id { text } =
    Backend.Interop.atomicOpCheck op { key = keys.waypoint id, versionstamp = Nothing }
        |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = keys.waypoint id, value = encodeWaypoint { text = text, completed = False } })


waypointDelete : Backend.Interop.AtomicOperation -> Api.WaypointId -> ConcurrentTask.ConcurrentTask x ()
waypointDelete op id =
    Backend.Interop.atomicOpDelete op { key = keys.waypoint id }


waypointSetCompleted kv op id completed =
    let
        key =
            keys.waypoint id
    in
    kvGet kv key decodeWaypoint WaypointSetCompletedDecodeError
        |> ConcurrentTask.andThen
            (\entry ->
                case entry of
                    Nothing ->
                        ConcurrentTask.fail WaypointSetCompletedMissingWaypoint

                    Just { versionstamp, value } ->
                        Backend.Interop.atomicOpCheck op { key = key, versionstamp = Just versionstamp }
                            |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = key, value = encodeWaypoint { value | completed = completed } })
            )


kvGet kv key decoder tagError =
    Backend.Interop.kvGet kv key
        |> ConcurrentTask.andThen
            (\result ->
                case result of
                    Nothing ->
                        ConcurrentTask.succeed Nothing

                    Just { value, versionstamp } ->
                        case Json.Decode.decodeValue decoder value of
                            Ok decoded ->
                                ConcurrentTask.succeed (Just { value = decoded, versionstamp = versionstamp })

                            Err err ->
                                ConcurrentTask.fail (tagError err)
            )


waypointsList kv =
    Backend.Interop.kvList kv keys.waypointPrefix decodeWaypoint
        |> ConcurrentTask.andThen Backend.Interop.iteratorToList
        |> ConcurrentTask.andThen
            (\items ->
                let
                    ( values, errors ) =
                        items
                            |> List.map
                                (\item ->
                                    case item.key of
                                        [ Backend.Interop.StringKvKeyPart "waypoints", Backend.Interop.StringKvKeyPart id ] ->
                                            case Json.Decode.decodeValue decodeWaypoint item.value of
                                                Ok value ->
                                                    Ok { id = Api.WaypointId id, waypoint = value }

                                                Err error ->
                                                    Err (WaypointsListDecodeError { id = Api.WaypointId id, error = error })

                                        _ ->
                                            Err (WaypointsListUnexpectedKey item.key)
                                )
                            |> Result.Extra.partition
                in
                case errors of
                    [] ->
                        ConcurrentTask.succeed values

                    head :: tail ->
                        ConcurrentTask.fail (Errors.construct head tail)
            )


decodePriorities =
    Json.Decode.field "priorities" (Json.Decode.list (Json.Decode.map Api.wrapWaypointId Json.Decode.string))


decodeWaypoint =
    Json.Decode.map2
        (\text completed -> { text = text, completed = completed })
        (Json.Decode.field "text" Json.Decode.string)
        (optionalField "completed" Json.Decode.bool
            |> Json.Decode.map (Maybe.withDefault False)
        )


encodeWaypoint { text, completed } =
    Json.Encode.object
        [ ( "text", Json.Encode.string text )
        , ( "completed", Json.Encode.bool completed )
        ]


optionalField name decoder =
    Json.Decode.oneOf
        [ Json.Decode.field name (Json.Decode.succeed True)
        , Json.Decode.succeed False
        ]
        |> Json.Decode.andThen
            (\isPresent ->
                if isPresent then
                    Json.Decode.map Just (Json.Decode.field name decoder)

                else
                    Json.Decode.succeed Nothing
            )


keys =
    { priorities = [ Backend.Interop.StringKvKeyPart "priorities" ]
    , waypoint =
        \id ->
            [ Backend.Interop.StringKvKeyPart "waypoints"
            , Backend.Interop.StringKvKeyPart (Api.unwrapWaypointId id)
            ]
    , waypointPrefix = [ Backend.Interop.StringKvKeyPart "waypoints" ]
    }
