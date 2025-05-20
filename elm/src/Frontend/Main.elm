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
        , sccNodeIds : KeyDict.KeyDict Api.WaypointId String Graph.NodeId
        , waypoints : KeyDict.KeyDict Api.WaypointId String Api.Waypoint
        , graph : Graph.Graph (Graph.Graph Api.WaypointId ()) ()
        }


type alias Model =
    { data : RemoteData
    , input : String
    , outstanding : Int
    , selected : Maybe Api.WaypointId
    }


type Msg
    = InitWaypoints (Result Http.Error (KeyDict.KeyDict Api.WaypointId String Api.Waypoint))
    | InitPriorities (Result Http.Error (List Api.WaypointId))
    | InputUpdate String
    | Select (Maybe Api.WaypointId)
    | AddWaypoint
    | AddWaypointFinished (Result Http.Error { id : Api.WaypointId, waypoint : Api.Waypoint })


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
      , input = ""
      , selected = Nothing
      , outstanding = 0
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

        InputUpdate input ->
            ( { model
                | input = input
              }
            , Cmd.none
            )

        AddWaypoint ->
            ( { model
                | input = ""
                , outstanding = model.outstanding + 1
              }
            , Endpoint.request Api.addWaypoint { text = model.input } AddWaypointFinished
            )

        AddWaypointFinished result ->
            ( { model
                | data =
                    case model.data of
                        Loading _ ->
                            Error

                        Error ->
                            model.data

                        Data data ->
                            case result of
                                Ok { id, waypoint } ->
                                    buildData data.priorities (Api.waypointIdKeyDict .insert id waypoint data.waypoints)

                                Err _ ->
                                    Error
                , outstanding = model.outstanding - 1
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
            buildData priorities waypoints

        _ ->
            Loading loading


buildData priorities waypoints =
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

        stronglyConnectedComponents =
            case Graph.stronglyConnectedComponents graph of
                Ok _ ->
                    graph
                        |> Graph.nodeIds
                        |> List.map (\nodeId -> Graph.inducedSubgraph [ nodeId ] graph)

                Err sccs ->
                    sccs

        nodeIdToSccNodeId =
            stronglyConnectedComponents
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( sccNodeId, scc ) acc ->
                        scc
                            |> Graph.nodeIds
                            |> List.foldl (\nodeId -> IntDict.insert nodeId sccNodeId) acc
                    )
                    IntDict.empty

        sccNodeIds =
            nodeIds
                |> Api.waypointIdKeyDict .foldl
                    (\waypointId nodeId acc ->
                        case IntDict.get nodeId nodeIdToSccNodeId of
                            Nothing ->
                                acc

                            Just sccNodeId ->
                                Api.waypointIdKeyDict .insert waypointId sccNodeId acc
                    )
                    (Api.waypointIdKeyDict .empty)

        sccGraph =
            Graph.fromNodeLabelsAndEdgePairs
                stronglyConnectedComponents
                (stronglyConnectedComponents
                    |> List.indexedMap
                        (\sccNodeId ->
                            Graph.fold
                                (\{ node } ->
                                    Graph.get node.id graph
                                        |> Maybe.map
                                            (\{ outgoing } ->
                                                outgoing
                                                    |> IntDict.keys
                                                    |> List.filterMap
                                                        (\outNodeId ->
                                                            nodeIdToSccNodeId
                                                                |> IntDict.get outNodeId
                                                                |> Maybe.Extra.filter ((/=) sccNodeId)
                                                        )
                                                    |> List.map (Tuple.pair sccNodeId)
                                            )
                                        |> Maybe.withDefault []
                                        |> List.append
                                )
                                []
                        )
                    |> List.concat
                )
    in
    Data
        { sccNodeIds = sccNodeIds
        , priorities = priorities
        , waypoints = waypoints
        , graph = sccGraph
        }


graphToString =
    Graph.toString (Api.unwrapWaypointId >> Just) (always Nothing)


sccGraphToString =
    Graph.toString (graphToString >> Just) (always Nothing)


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title.main
    , body =
        Ui.global
            |> Ui.append
                (case model.data of
                    Data data ->
                        Ui.scaffold
                            (viewWaypoints model data)
                            (Ui.input { text = model.input, onInput = InputUpdate }
                                |> Ui.append
                                    (if model.input == "" then
                                        Ui.empty

                                     else
                                        Ui.button AddWaypoint consts.strings.add
                                    )
                            )

                    Error ->
                        Ui.alert Strings.error

                    Loading _ ->
                        Ui.empty
                )
            |> Ui.toHtml
    }


globalFilter input waypoint =
    let
        words =
            String.words input
                |> List.map String.toLower

        text =
            waypoint
                |> Maybe.map .text
                |> Maybe.withDefault ""
                |> String.toLower

        url =
            waypoint
                |> Maybe.andThen .url
                |> Maybe.withDefault ""
                |> String.toLower
    in
    words
        |> List.all
            (\word ->
                String.contains word text || String.contains word url
            )


viewWaypoints { input, selected } { priorities, sccNodeIds, graph, waypoints } =
    let
        selectedSeeds =
            graph
                |> Graph.nodes
                |> List.filter
                    (\n ->
                        Graph.nodes n.label
                            |> List.any (\{ label } -> Just label == selected)
                    )
                |> List.map .id

        transitiveRequires =
            Graph.guidedDfs
                Graph.alongOutgoingEdges
                (Graph.onDiscovery
                    (\{ node } acc ->
                        Graph.nodes node.label
                            |> List.foldl (\{ label } -> Api.waypointIdKeyDict .insert label ()) acc
                    )
                )
                selectedSeeds
                (Api.waypointIdKeyDict .empty)
                graph
                |> Tuple.first

        transitiveRequiredBy =
            Graph.guidedDfs
                Graph.alongIncomingEdges
                (Graph.onDiscovery
                    (\{ node } acc ->
                        Graph.nodes node.label
                            |> List.foldl (\{ label } -> Api.waypointIdKeyDict .insert label ()) acc
                    )
                )
                selectedSeeds
                (Api.waypointIdKeyDict .empty)
                graph
                |> Tuple.first
    in
    Ui.list
        (graph
            |> squeeze priorities sccNodeIds
            |> List.concatMap
                (\scc ->
                    Graph.dfs (Graph.onDiscovery (.node >> .label >> (::))) [] scc
                        |> List.filterMap
                            (\id ->
                                let
                                    highlight =
                                        if Just id == selected then
                                            Just Ui.primary

                                        else if Api.waypointIdKeyDict .member id transitiveRequires || Api.waypointIdKeyDict .member id transitiveRequiredBy then
                                            Just Ui.secondary

                                        else if Graph.size scc > 1 then
                                            Just Ui.conflict

                                        else
                                            Nothing

                                    maybeWaypoint =
                                        Api.waypointIdKeyDict .get id waypoints

                                    passesFilter =
                                        maybeWaypoint
                                            |> globalFilter input
                                in
                                if passesFilter then
                                    Just
                                        (case maybeWaypoint of
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

                                else
                                    Nothing
                            )
                )
        )


squeeze priorities sccNodeIds graph =
    let
        nodePriorities =
            priorities
                |> List.indexedMap
                    (\priorityIndex priorityWaypointId ->
                        { transitiveSccNodeIds =
                            Graph.guidedDfs
                                Graph.alongOutgoingEdges
                                ((\{ node } -> (::) node.id) |> Graph.onDiscovery)
                                ([ Api.waypointIdKeyDict .get priorityWaypointId sccNodeIds ] |> List.filterMap identity)
                                []
                                graph
                                |> Tuple.first
                        , priorityIndex = priorityIndex
                        }
                    )
                |> List.foldr
                    (\{ priorityIndex, transitiveSccNodeIds } acc ->
                        List.foldl
                            (\nodeId ->
                                IntDict.update
                                    nodeId
                                    (Maybe.withDefault [] >> (::) (List.length priorities - priorityIndex) >> Just)
                            )
                            acc
                            transitiveSccNodeIds
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


subscriptions model =
    Sub.none


consts =
    { strings =
        { add = "+"
        }
    }
