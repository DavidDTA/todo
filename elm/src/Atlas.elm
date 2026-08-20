module Atlas exposing
    ( Atlas
    , build
    , incoming
    , outgoing
    , transitiveIncoming
    , transitiveOutgoing
    , waypointGroup
    , waypointGroups
    , waypointIdKeyDict
    )

import Graph
import IntDict
import KeyDict
import Lamdera.Wire3
import List
import Maybe.Extra
import WaypointId


type Atlas
    = Atlas
        { graph : Graph.Graph WaypointId.WaypointId ()
        , sccGraph : Graph.Graph (Graph.Graph WaypointId.WaypointId ()) ()
        , nodeIds : KeyDict.KeyDict WaypointId.WaypointId String Int
        , sccNodeIds : KeyDict.KeyDict WaypointId.WaypointId String Int
        }


waypointIdKeyDict =
    KeyDict.define WaypointId.WaypointId (\(WaypointId.WaypointId id) -> id)


build waypoints dependencies =
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
            List.foldl
                addIfMissing
                (waypointIdKeyDict .empty)
                (waypoints ++ List.concatMap (\{ from, to } -> [ from, to ]) dependencies)

        graph =
            Graph.fromNodesAndEdges
                (waypointIdKeyDict .foldl
                    (\waypointId nodeId -> (::) { id = nodeId, label = waypointId })
                    []
                    nodeIds
                )
                (dependencies
                    |> List.filterMap
                        (\edge -> Maybe.map2 (\from to -> { from = from, to = to, label = () }) (waypointIdKeyDict .get edge.from nodeIds) (waypointIdKeyDict .get edge.to nodeIds))
                )

        stronglyConnectedComponents =
            case Graph.stronglyConnectedComponents graph of
                Ok _ ->
                    graph
                        |> Graph.nodeIds
                        |> List.map (\nodeId -> Graph.inducedSubgraph [ nodeId ] graph)

                Err sccs ->
                    sccs

        sccNodeIds =
            nodeIds
                |> waypointIdKeyDict .foldl
                    (\waypointId nodeId acc ->
                        case IntDict.get nodeId nodeIdToSccNodeId of
                            Nothing ->
                                acc

                            Just sccNodeId ->
                                waypointIdKeyDict .insert waypointId sccNodeId acc
                    )
                    (waypointIdKeyDict .empty)

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

        sccGraph =
            Graph.fromNodeLabelsAndEdgePairs
                stronglyConnectedComponents
                (stronglyConnectedComponents
                    |> List.indexedMap
                        (\sccNodeId ->
                            Graph.fold
                                (\contrxt ->
                                    Graph.get contrxt.node.id graph
                                        |> Maybe.map
                                            (\node ->
                                                node.outgoing
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
    Atlas
        { graph = graph
        , sccGraph = sccGraph
        , nodeIds = nodeIds
        , sccNodeIds = sccNodeIds
        }


waypointGroups (Atlas { sccGraph }) =
    sccGraph
        |> Graph.nodes
        |> List.map
            (\{ label } ->
                Graph.fold
                    (\{ node } -> waypointIdKeyDict .insert node.label {})
                    (waypointIdKeyDict .empty)
                    label
            )


waypointGroup waypointId (Atlas { sccNodeIds, sccGraph }) =
    waypointIdKeyDict .get waypointId sccNodeIds
        |> Maybe.andThen (\sccNodeId -> Graph.get sccNodeId sccGraph)
        |> Maybe.map .node
        |> Maybe.map .label
        |> Maybe.map Graph.nodes
        |> Maybe.withDefault []
        |> List.foldl (\{ label } -> waypointIdKeyDict .insert label {}) (waypointIdKeyDict .empty)


incoming =
    help .incoming


outgoing =
    help .outgoing


help direction waypointId (Atlas { graph, nodeIds }) =
    waypointIdKeyDict .get waypointId nodeIds
        |> Maybe.andThen (\nodeId -> Graph.get nodeId graph)
        |> Maybe.map direction
        |> Maybe.withDefault IntDict.empty
        |> IntDict.keys
        |> List.filterMap (\nodeId -> Graph.get nodeId graph)
        |> List.map .node
        |> List.map .label
        |> List.foldl (\id -> waypointIdKeyDict .insert id {}) (waypointIdKeyDict .empty)


transitiveIncoming =
    transitiveHelp Graph.alongIncomingEdges


transitiveOutgoing =
    transitiveHelp Graph.alongOutgoingEdges


transitiveHelp neighborSelector waypointId (Atlas { graph, nodeIds }) =
    Graph.guidedDfs neighborSelector (Graph.onDiscovery (\{ node } acc -> waypointIdKeyDict .insert node.label {} acc)) (Maybe.Extra.unwrap [] List.singleton (waypointIdKeyDict .get waypointId nodeIds)) (waypointIdKeyDict .empty) graph
        |> Tuple.first
