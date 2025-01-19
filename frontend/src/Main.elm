module Main exposing (main)

import Browser
import Css
import Dict
import Graph
import Html.Styled
import Html.Styled.Attributes
import Http
import IntDict
import Json.Decode
import KeyDict
import List.Extra
import Strings


type WaypointId
    = WaypointId String


type alias Waypoint =
    { text : String
    , completed : Bool
    , url : Maybe String
    , requires : List WaypointId
    , requiredBy : List WaypointId
    }


type RemoteData a
    = Loading
    | Error
    | Data a


type alias Model =
    { waypoints :
        RemoteData
            { nodeIds : KeyDict.KeyDict WaypointId String Graph.NodeId
            , graph :
                Result
                    (List
                        { id : WaypointId
                        , value : Maybe Waypoint
                        }
                    )
                    (Graph.AcyclicGraph
                        { id : WaypointId
                        , value : Maybe Waypoint
                        }
                        ()
                    )
            }
    }


type Msg
    = InitWaypoints (Result Http.Error (KeyDict.KeyDict WaypointId String Waypoint))


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { waypoints = Loading }
    , Http.get
        { url = "/-/api/waypoints"
        , expect = Http.expectJson InitWaypoints decodeWaypoints
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitWaypoints result ->
            ( { model
                | waypoints =
                    case result of
                        Err _ ->
                            Error

                        Ok waypoints ->
                            let
                                addIfMissing id dict =
                                    waypointIdKeyDict .update
                                        id
                                        (\current ->
                                            case current of
                                                Nothing ->
                                                    Just (waypointIdKeyDict .size dict)

                                                Just _ ->
                                                    current
                                        )
                                        dict

                                nodeIds =
                                    waypointIdKeyDict .foldl
                                        (\k v acc ->
                                            k
                                                :: (v.requires ++ v.requiredBy)
                                                |> List.foldl addIfMissing acc
                                        )
                                        (waypointIdKeyDict .empty)
                                        waypoints
                            in
                            { nodeIds = nodeIds
                            , graph =
                                Graph.fromNodesAndEdges
                                    (waypointIdKeyDict .foldl
                                        (\waypointId nodeId -> (::) { id = nodeId, label = { id = waypointId, value = waypointIdKeyDict .get waypointId waypoints } })
                                        []
                                        nodeIds
                                    )
                                    (waypointIdKeyDict .foldl
                                        (\waypointId waypoint acc ->
                                            case waypointIdKeyDict .get waypointId nodeIds of
                                                Nothing ->
                                                    acc

                                                Just nodeId ->
                                                    List.filterMap
                                                        (\requirementWaypointId ->
                                                            waypointIdKeyDict .get requirementWaypointId nodeIds
                                                                |> Maybe.map (\requirementNodeId -> { from = nodeId, to = requirementNodeId, label = () })
                                                        )
                                                        waypoint.requires
                                                        ++ List.filterMap
                                                            (\requiredByWaypointId ->
                                                                waypointIdKeyDict .get requiredByWaypointId nodeIds
                                                                    |> Maybe.map (\requiredByNodeId -> { from = requiredByNodeId, to = nodeId, label = () })
                                                            )
                                                            waypoint.requiredBy
                                                        ++ acc
                                        )
                                        []
                                        waypoints
                                    )
                                    |> Graph.stronglyConnectedComponents
                                    |> Result.mapError
                                        (List.Extra.findMap extractCycleFromStronglyConnectedComponent)
                                    |> Result.mapError (Maybe.withDefault [])
                            }
                                |> Data
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title.main
    , body =
        (case model.waypoints of
            Loading ->
                []

            Error ->
                [ Html.Styled.text Strings.error ]

            Data waypoints ->
                case waypoints.graph of
                    Err cycle ->
                        [ Html.Styled.div [] [ Html.Styled.text Strings.cycleDetected ]
                        , Html.Styled.ul
                            []
                            (cycle
                                |> List.map
                                    (\{ id, value } ->
                                        viewWaypointRowPrimitive
                                            { text =
                                                value
                                                    |> Maybe.map .text
                                                    |> Maybe.withDefault
                                                        Strings.unknownWaypoint
                                            , icon = "↳"
                                            }
                                    )
                            )
                        ]

                    Ok acyclic ->
                        [ Html.Styled.ul
                            []
                            (acyclic
                                |> Graph.topologicalSort
                                |> List.reverse
                                |> List.map .node
                                |> List.map .label
                                |> List.map
                                    (\{ id, value } ->
                                        case value of
                                            Just waypoint ->
                                                viewWaypointRow waypoint

                                            Nothing ->
                                                viewWaypointRowPrimitive { text = Strings.unknownWaypoint, icon = "⍰" }
                                    )
                            )
                        ]
        )
            |> List.map Html.Styled.toUnstyled
    }


viewWaypointRow { text, completed } =
    viewWaypointRowPrimitive
        { text = text
        , icon =
            if completed then
                "☑"

            else
                "☐"
        }


viewWaypointRowPrimitive { text, icon } =
    Html.Styled.li
        [ Html.Styled.Attributes.css
            [ Css.listStyleType
                (Css.string (icon ++ " "))
            ]
        ]
        [ Html.Styled.text text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


extractCycleFromStronglyConnectedComponent graph =
    Graph.bfs
        (\path _ acc ->
            case ( acc, List.head path, List.Extra.last (Maybe.withDefault [] (List.tail path)) ) of
                ( Just _, _, _ ) ->
                    acc

                ( _, Just current, Just root ) ->
                    if IntDict.member root.node.id current.outgoing then
                        path
                            |> List.map .node
                            |> List.map .label
                            |> List.reverse
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
        )
        Nothing
        graph


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


waypointIdKeyDict =
    KeyDict.define WaypointId (\(WaypointId str) -> str)
