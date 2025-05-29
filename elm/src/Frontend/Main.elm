module Frontend.Main exposing (main)

import Api
import Browser
import Css
import Dict
import Endpoint
import Graph
import Heap
import Http
import IntDict
import Json.Decode
import KeyDict
import List.Extra
import Maybe.Extra
import Strings
import Ui


type RemoteData
    = Loading
        { priorities : Maybe (List Api.WaypointId)
        , waypoints : Maybe (KeyDict.KeyDict Api.WaypointId String Api.Waypoint)
        }
    | Error
    | Data
        { priorities : List Api.WaypointId
        , nodeIds : KeyDict.KeyDict Api.WaypointId String Graph.NodeId
        , waypoints : KeyDict.KeyDict Api.WaypointId String Api.Waypoint
        , graph : Graph.Graph Api.WaypointId ()
        , acyclic :
            Result
                (List Api.WaypointId)
                (Graph.AcyclicGraph Api.WaypointId ())
        }


type alias Model =
    { data : RemoteData
    , selected : Maybe Api.WaypointId
    }


type Msg
    = InitWaypoints (Result Http.Error (KeyDict.KeyDict Api.WaypointId String Api.Waypoint))
    | InitPriorities (Result Http.Error (List Api.WaypointId))
    | Select (Maybe Api.WaypointId)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { data = Loading { priorities = Nothing, waypoints = Nothing }
      , selected = Nothing
      }
    , Cmd.batch
        [ Endpoint.request Api.waypoints InitWaypoints
        , Endpoint.request Api.priorities InitPriorities
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitPriorities result ->
            ( { model
                | data =
                    initData
                        (\value loading -> { loading | priorities = value })
                        result
                        model.data
              }
            , Cmd.none
            )

        InitWaypoints result ->
            ( { model
                | data =
                    initData
                        (\value loading -> { loading | waypoints = value })
                        result
                        model.data
              }
            , Cmd.none
            )

        Select selection ->
            ( { model
                | selected = selection
              }
            , Cmd.none
            )


initData updateLoading result data =
    case ( data, result ) of
        ( Loading loading, Ok value ) ->
            if updateLoading Nothing loading == loading then
                loading
                    |> updateLoading (Just value)
                    |> resolveData

            else
                Error

        _ ->
            Error


resolveData loading =
    case ( loading.priorities, loading.waypoints ) of
        ( Just priorities, Just waypoints ) ->
            let
                addIfMissing id dict =
                    Api.waypointIdKeyDict .update
                        id
                        (\current ->
                            case current of
                                Nothing ->
                                    Just (Api.waypointIdKeyDict .size dict)

                                Just _ ->
                                    current
                        )
                        dict

                nodeIds =
                    List.foldl
                        addIfMissing
                        (Api.waypointIdKeyDict .empty)
                        (priorities
                            ++ Api.waypointIdKeyDict .foldl (\k v acc -> k :: v.requires ++ v.requiredBy ++ acc) [] waypoints
                        )

                graph =
                    Graph.fromNodesAndEdges
                        (Api.waypointIdKeyDict .foldl
                            (\waypointId nodeId -> (::) { id = nodeId, label = waypointId })
                            []
                            nodeIds
                        )
                        (Api.waypointIdKeyDict .foldl
                            (\waypointId waypoint acc ->
                                List.map
                                    (\requirementWaypointId ->
                                        { from = waypointId, to = requirementWaypointId }
                                    )
                                    waypoint.requires
                                    ++ List.map
                                        (\requiredByWaypointId ->
                                            { from = requiredByWaypointId, to = waypointId }
                                        )
                                        waypoint.requiredBy
                                    ++ acc
                            )
                            []
                            waypoints
                            |> List.filterMap
                                (\edge -> Maybe.map2 (\from to -> { from = from, to = to, label = () }) (Api.waypointIdKeyDict .get edge.from nodeIds) (Api.waypointIdKeyDict .get edge.to nodeIds))
                        )
            in
            Data
                { nodeIds = nodeIds
                , priorities = priorities
                , waypoints = waypoints
                , graph = graph
                , acyclic =
                    graph
                        |> Graph.stronglyConnectedComponents
                        |> Result.mapError
                            (List.Extra.findMap extractCycleFromStronglyConnectedComponent)
                        |> Result.mapError (Maybe.withDefault [])
                }

        _ ->
            Loading loading


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title.main
    , body =
        (case model.data of
            Data data ->
                case data.acyclic of
                    Err cycle ->
                        viewWaypointsCycle cycle data.waypoints

                    Ok _ ->
                        viewWaypointsAcyclic model.selected data

            Error ->
                Ui.alert Strings.error

            Loading _ ->
                Ui.empty
        )
            |> Ui.toHtml
    }


viewWaypointsCycle cycle waypoints =
    Ui.heading Strings.cycleDetected
        |> Ui.append
            (cycle
                |> List.map
                    (\id ->
                        let
                            waypoint =
                                Api.waypointIdKeyDict .get id waypoints
                        in
                        viewWaypointRowPrimitive
                            { text =
                                waypoint
                                    |> Maybe.map .text
                                    |> Maybe.withDefault
                                        Strings.unknownWaypoint
                            , icon = "↳"
                            , id = id
                            , highlight = Nothing
                            , url =
                                waypoint
                                    |> Maybe.andThen .url
                            }
                    )
                |> Ui.list
            )


viewWaypointsAcyclic selected { priorities, nodeIds, graph, waypoints } =
    let
        seeds =
            graph
                |> Graph.nodes
                |> List.filter (\{ label } -> Just label == selected)
                |> List.map .id

        transitiveRequires =
            Graph.guidedDfs
                Graph.alongOutgoingEdges
                (Graph.onDiscovery (\{ node } -> Api.waypointIdKeyDict .insert node.label ()))
                seeds
                (Api.waypointIdKeyDict .empty)
                graph
                |> Tuple.first

        transitiveRequiredBy =
            Graph.guidedDfs
                Graph.alongIncomingEdges
                (Graph.onDiscovery (\{ node } -> Api.waypointIdKeyDict .insert node.label ()))
                seeds
                (Api.waypointIdKeyDict .empty)
                graph
                |> Tuple.first
    in
    Ui.list
        (graph
            |> squeeze priorities nodeIds
            |> List.map
                (\id ->
                    let
                        highlight =
                            if Just id == selected then
                                Just Ui.primary

                            else if Api.waypointIdKeyDict .member id transitiveRequires || Api.waypointIdKeyDict .member id transitiveRequiredBy then
                                Just Ui.secondary

                            else
                                Nothing
                    in
                    case Api.waypointIdKeyDict .get id waypoints of
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
                                , icon = "﹖"
                                , id = id
                                , highlight = highlight
                                , url = Nothing
                                }
                )
        )



{- assumes graph is acyclic -}


squeeze priorities nodeIds graph =
    let
        nodePriorities =
            priorities
                |> List.indexedMap
                    (\priorityIndex priorityWaypointId ->
                        { transitiveNodeIds =
                            Graph.guidedDfs
                                Graph.alongOutgoingEdges
                                ((\{ node } -> (::) node.id) |> Graph.onDiscovery)
                                ([ Api.waypointIdKeyDict .get priorityWaypointId nodeIds ] |> List.filterMap identity)
                                []
                                graph
                                |> Tuple.first
                        , priorityIndex = priorityIndex
                        }
                    )
                |> List.foldr
                    (\{ priorityIndex, transitiveNodeIds } acc ->
                        List.foldl
                            (\nodeId ->
                                IntDict.update
                                    nodeId
                                    (Maybe.withDefault [] >> (::) (List.length priorities - priorityIndex) >> Just)
                            )
                            acc
                            transitiveNodeIds
                    )
                    IntDict.empty

        initialQueue =
            Graph.fold
                (\{ node, outgoing } acc ->
                    if IntDict.isEmpty outgoing then
                        Heap.push node.id acc

                    else
                        acc
                )
                (Heap.empty
                    (Heap.biggest
                        |> Heap.by
                            (\nodeId ->
                                IntDict.get nodeId nodePriorities |> Maybe.withDefault []
                            )
                    )
                )
                graph

        step queue selected stepAcc =
            case Heap.pop queue of
                Nothing ->
                    stepAcc

                Just ( head, tail ) ->
                    let
                        updatedSelected =
                            IntDict.insert head () selected

                        updatedQueue =
                            case Graph.get head graph of
                                Nothing ->
                                    tail

                                Just { incoming } ->
                                    IntDict.foldl
                                        (\incomingNodeId _ acc ->
                                            case Graph.get incomingNodeId graph of
                                                Nothing ->
                                                    acc

                                                Just { node, outgoing } ->
                                                    if IntDict.isEmpty (IntDict.diff outgoing updatedSelected) then
                                                        Heap.push node.id acc

                                                    else
                                                        acc
                                        )
                                        tail
                                        incoming
                    in
                    step updatedQueue updatedSelected (head :: stepAcc)
    in
    step initialQueue IntDict.empty []
        |> List.filterMap
            (\nodeId ->
                Graph.get nodeId graph
                    |> Maybe.map (.node >> .label)
            )
        |> List.reverse


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
    { bullet = icon
    , highlight = highlight
    , onEnter = Select (Just id)
    , onExit = Select Nothing
    , content =
        Ui.text text
            |> Ui.append
                (case url of
                    Nothing ->
                        Ui.empty

                    Just justUrl ->
                        Ui.text " "
                            |> Ui.append (Ui.link justUrl)
                )
    }


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
