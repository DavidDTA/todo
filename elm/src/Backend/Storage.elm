module Backend.Storage exposing
    ( PrioritiesListError(..)
    , WaypointSetCompletedError(..)
    , WaypointSetPriority2Error(..)
    , WaypointSetPriorityError(..)
    , WaypointsListError(..)
    , prioritiesList
    , waypointAdd
    , waypointDelete
    , waypointSetCompleted
    , waypointSetPriority
    , waypointSetPriority2
    , waypointsList
    )

import Api
import Backend.Interop
import ConcurrentTask
import Errors
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Result.Extra


type WaypointsListError
    = WaypointsListUnexpectedKey Backend.Interop.Log
    | WaypointsListDecodeError Backend.Interop.Log


type PrioritiesListError
    = PrioritiesListDecodeError Backend.Interop.Log


type WaypointSetCompletedError
    = WaypointSetCompletedMissingWaypoint Backend.Interop.Log
    | WaypointSetCompletedDecodeError Backend.Interop.Log


type WaypointSetPriorityError
    = WaypointSetPriorityDecodeError Backend.Interop.Log


type WaypointSetPriority2Error
    = WaypointSetPriority2MissingWaypoint Backend.Interop.Log
    | WaypointSetPriority2DecodeError Backend.Interop.Log


prioritiesList kv =
    kvGet kv keys.priorities decodePriorities PrioritiesListDecodeError
        |> ConcurrentTask.map (Maybe.map .value >> Maybe.withDefault [])


waypointAdd : Backend.Interop.AtomicOperation -> Api.WaypointId -> { text : String } -> ConcurrentTask.ConcurrentTask x ()
waypointAdd op id { text } =
    Backend.Interop.atomicOpCheck op { key = keys.waypoint id, versionstamp = Nothing }
        |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = keys.waypoint id, value = encodeWaypoint { text = text, completed = False, priority = Nothing } })


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
                        ConcurrentTask.fail (WaypointSetCompletedMissingWaypoint (logMissingValue key))

                    Just { versionstamp, value } ->
                        Backend.Interop.atomicOpCheck op { key = key, versionstamp = Just versionstamp }
                            |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = key, value = encodeWaypoint { value | completed = completed } })
            )


waypointSetPriority kv op id maybePriority =
    kvGet kv keys.priorities decodePriorities WaypointSetPriorityDecodeError
        |> ConcurrentTask.andThen
            (\entry ->
                let
                    newPriorities =
                        entry
                            |> Maybe.map .value
                            |> Maybe.withDefault []
                            |> List.filter ((/=) id)
                            |> (\filtered ->
                                    case maybePriority of
                                        Nothing ->
                                            filtered

                                        Just priority ->
                                            filtered
                                                |> List.Extra.splitAt priority
                                                |> (\( head, tail ) -> head ++ id :: tail)
                               )
                in
                Backend.Interop.atomicOpCheck op { key = keys.priorities, versionstamp = Maybe.map .versionstamp entry }
                    |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = keys.priorities, value = encodePriorities newPriorities })
                    |> ConcurrentTask.return newPriorities
            )


waypointSetPriority2 kv op id priority =
    let
        key =
            keys.waypoint id
    in
    kvGet kv key decodeWaypoint WaypointSetPriority2DecodeError
        |> ConcurrentTask.andThen
            (\entry ->
                case entry of
                    Nothing ->
                        ConcurrentTask.fail (WaypointSetPriority2MissingWaypoint (logMissingValue key))

                    Just { versionstamp, value } ->
                        Backend.Interop.atomicOpCheck op { key = key, versionstamp = Just versionstamp }
                            |> ConcurrentTask.andThenDo (Backend.Interop.atomicOpSet op { key = key, value = encodeWaypoint { value | priority = priority } })
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
                                ConcurrentTask.fail (tagError (logKvDecodeError key err))
            )


logKvDecodeError key error =
    [ Json.Encode.string "Error decoding kv value"
    , Json.Encode.string "key:"
    , Backend.Interop.encodeKvKey key
    , Json.Encode.string "errror:"
    , Json.Encode.string (Json.Decode.errorToString error)
    ]


logMissingValue key =
    [ Json.Encode.string "No value for key"
    , Backend.Interop.encodeKvKey key
    ]


logUnexpectedKey key =
    [ Json.Encode.string "Unexpected key while listing"
    , Backend.Interop.encodeKvKey key
    ]


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
                                                    Err (WaypointsListDecodeError (logKvDecodeError item.key error))

                                        _ ->
                                            Err
                                                (WaypointsListUnexpectedKey (logUnexpectedKey item.key))
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


encodePriorities priorities =
    Json.Encode.object
        [ ( "priorities"
          , Json.Encode.list (Api.unwrapWaypointId >> Json.Encode.string) priorities
          )
        ]


decodeWaypoint =
    Json.Decode.map3
        (\text completed priority -> { text = text, completed = completed, priority = priority })
        (Json.Decode.field "text" Json.Decode.string)
        (optionalField "completed" Json.Decode.bool
            |> Json.Decode.map (Maybe.withDefault False)
        )
        (optionalField "priority" (Json.Decode.nullable Json.Decode.string)
            |> Json.Decode.map (Maybe.withDefault Nothing)
        )


encodeWaypoint { text, completed, priority } =
    Json.Encode.object
        [ ( "text", Json.Encode.string text )
        , ( "completed", Json.Encode.bool completed )
        , ( "priority", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string priority )
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
