module Main exposing (main)

import Browser
import Css
import Dict
import Graph
import Html.Events.Extra.Pointer
import Html.Styled
import Html.Styled.Attributes
import Http
import IntDict
import Json.Decode
import KeyDict
import List.Extra
import Maybe.Extra
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
    { selected : Maybe WaypointId
    , waypoints :
        RemoteData
            { nodeIds : KeyDict.KeyDict WaypointId String Graph.NodeId
            , graph :
                Result
                    (List
                        { id : WaypointId
                        , value : Maybe Waypoint
                        }
                    )
                    { graph :
                        Graph.Graph
                            { id : WaypointId
                            , value : Maybe Waypoint
                            }
                            ()
                    , acyclic :
                        Graph.AcyclicGraph
                            { id : WaypointId
                            , value : Maybe Waypoint
                            }
                            ()
                    }
            }
    }


type Msg
    = InitWaypoints (Result Http.Error (KeyDict.KeyDict WaypointId String Waypoint))
    | Select (Maybe WaypointId)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { selected = Nothing
      , waypoints = Loading
      }
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
                                    |> (\graph ->
                                            graph
                                                |> Graph.stronglyConnectedComponents
                                                |> Result.mapError
                                                    (List.Extra.findMap extractCycleFromStronglyConnectedComponent)
                                                |> Result.mapError (Maybe.withDefault [])
                                                |> Result.map
                                                    (\acyclic ->
                                                        { graph = graph
                                                        , acyclic = acyclic
                                                        }
                                                    )
                                       )
                            }
                                |> Data
              }
            , Cmd.none
            )

        Select selection ->
            ( { model
                | selected = selection
              }
            , Cmd.none
            )


type WaypointRowHighlight
    = NoHighlight
    | Selected
    | DescendantOrAncestor


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
                        viewWaypointsCycle cycle

                    Ok acyclic ->
                        viewWaypointsAcyclic model.selected acyclic
        )
            |> List.map Html.Styled.toUnstyled
    }


viewWaypointsCycle cycle =
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
                        , id = id
                        , highlight = NoHighlight
                        , url =
                            value
                                |> Maybe.andThen .url
                        }
                )
        )
    ]


viewWaypointsAcyclic selected { graph, acyclic } =
    let
        seeds =
            graph
                |> Graph.nodes
                |> List.filter (\{ label } -> Just label.id == selected)
                |> List.map .id

        transitiveRequires =
            Graph.guidedDfs
                Graph.alongOutgoingEdges
                (Graph.onDiscovery (\{ node } -> waypointIdKeyDict .insert node.label.id ()))
                seeds
                (waypointIdKeyDict .empty)
                graph
                |> Tuple.first

        transitiveRequiredBy =
            Graph.guidedDfs
                Graph.alongIncomingEdges
                (Graph.onDiscovery (\{ node } -> waypointIdKeyDict .insert node.label.id ()))
                seeds
                (waypointIdKeyDict .empty)
                graph
                |> Tuple.first
    in
    [ Html.Styled.ul
        []
        (acyclic
            |> Graph.topologicalSort
            |> List.reverse
            |> List.map .node
            |> List.map .label
            |> List.map
                (\{ id, value } ->
                    let
                        highlight =
                            if Just id == selected then
                                Selected

                            else if waypointIdKeyDict .member id transitiveRequires || waypointIdKeyDict .member id transitiveRequiredBy then
                                DescendantOrAncestor

                            else
                                NoHighlight
                    in
                    case value of
                        Just waypoint ->
                            viewWaypointRow
                                { text = waypoint.text
                                , completed = waypoint.completed
                                , highlight = highlight
                                , id = id
                                , url = waypoint.url
                                }

                        Nothing ->
                            viewWaypointRowPrimitive
                                { text = Strings.unknownWaypoint
                                , icon = "⍰"
                                , id = id
                                , highlight = highlight
                                , url = Nothing
                                }
                )
        )
    ]


viewWaypointRow { completed, highlight, id, text, url } =
    viewWaypointRowPrimitive
        { text = text
        , icon =
            if completed then
                "☑"

            else
                "☐"
        , id = id
        , highlight = highlight
        , url = url
        }


viewWaypointRowPrimitive { highlight, icon, id, text, url } =
    Html.Styled.li
        [ Html.Styled.Attributes.css
            [ Css.listStyleType
                (Css.string (icon ++ " "))
            , Css.backgroundColor
                (case highlight of
                    NoHighlight ->
                        Css.unset

                    Selected ->
                        Css.rgb 255 255 128

                    DescendantOrAncestor ->
                        Css.rgb 160 160 255
                )
            ]
        , Html.Events.Extra.Pointer.onEnter (always (Select (Just id)))
            |> Html.Styled.Attributes.fromUnstyled
        , Html.Events.Extra.Pointer.onLeave (always (Select Nothing))
            |> Html.Styled.Attributes.fromUnstyled
        ]
        ([ Html.Styled.text text
         ]
            ++ (case url of
                    Nothing ->
                        []

                    Just justUrl ->
                        [ Html.Styled.text " "
                        , Html.Styled.a
                            [ Html.Styled.Attributes.href justUrl
                            ]
                            [ Html.Styled.text "🔗"
                            ]
                        ]
               )
        )


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
