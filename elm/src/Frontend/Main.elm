module Frontend.Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Css
import Dict
import Endpoint
import Graph
import Heap
import Html
import Http
import IntDict
import Json.Decode
import KeyDict
import List.Extra
import Maybe.Extra
import NetworkQueue
import Strings
import Task
import Ui
import Url


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
    , navigationKey : Browser.Navigation.Key
    , networkQueue : NetworkQueue.NetworkQueue NetworkRequest ()
    , screen : Screen
    }


type Screen
    = Home
    | WaypointDetail Api.WaypointId
    | Oops OopsReason


type Msg
    = DelayedInit
    | NetworkResponse { token : NetworkQueue.Token, result : Result.Result Http.Error NetworkResponse }
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | UserAction UserAction


type UserAction
    = ClickAddWaypoint
    | ClickDeleteWaypoint Api.WaypointId
    | ChangeInput String


type NetworkRequest
    = AddWaypoint { text : String }
    | DeleteWaypoint { id : Api.WaypointId }
    | InitWaypoints
    | InitPriorities


type NetworkResponse
    = InitWaypointsResponse (KeyDict.KeyDict Api.WaypointId String Api.Waypoint)
    | InitPrioritiesResponse (List Api.WaypointId)
    | AddWaypointResponse { id : Api.WaypointId, waypoint : Api.Waypoint }
    | DeleteWaypointResponse Api.WaypointId {}


type OopsReason
    = UnknownPath
    | DataError


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { data = Loading { priorities = Nothing, waypoints = Nothing }
      , input = ""
      , screen = parseScreen url.path
      , navigationKey = key
      , networkQueue = NetworkQueue.empty
      }
    , Task.perform identity (Task.succeed DelayedInit)
    )


parseScreen url =
    case Endpoint.splitPath url of
        Just [ "" ] ->
            Home

        Just [ "detail", id ] ->
            WaypointDetail (Api.WaypointId id)

        _ ->
            Oops UnknownPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DelayedInit ->
            ( model, Cmd.none )
                |> makeNetworkRequest InitWaypoints
                |> makeNetworkRequest InitPriorities

        NetworkResponse { token, result } ->
            case result of
                Err _ ->
                    let
                        { queue, cmd } =
                            NetworkQueue.halt requestToCmd token () model.networkQueue
                    in
                    ( { model | networkQueue = queue }, cmd )

                Ok response ->
                    let
                        { queue, cmd } =
                            NetworkQueue.dequeue requestToCmd token model.networkQueue
                    in
                    { model | networkQueue = queue }
                        |> updateForNetworkResponse response
                        |> (\( newModel, responseCmd ) -> ( newModel, Cmd.batch [ cmd, responseCmd ] ))

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        OnUrlChange url ->
            ( { model | screen = parseScreen url.path }, Cmd.none )

        UserAction userAction ->
            case userAction of
                ChangeInput input ->
                    ( { model
                        | input = input
                      }
                    , Cmd.none
                    )

                ClickAddWaypoint ->
                    ( { model
                        | input = ""
                      }
                    , Cmd.none
                    )
                        |> makeNetworkRequest (AddWaypoint { text = model.input })

                ClickDeleteWaypoint id ->
                    ( { model
                        | screen =
                            case model.screen of
                                Home ->
                                    model.screen

                                WaypointDetail detailId ->
                                    if id == detailId then
                                        Home

                                    else
                                        model.screen

                                Oops _ ->
                                    model.screen
                      }
                    , Cmd.none
                    )
                        |> makeNetworkRequest (DeleteWaypoint { id = id })


updateForNetworkResponse response model =
    case response of
        InitPrioritiesResponse value ->
            ( { model
                | data =
                    initData
                        (\loading -> { loading | priorities = Just value })
                        model.data
              }
            , Cmd.none
            )

        InitWaypointsResponse value ->
            ( { model
                | data =
                    initData
                        (\loading -> { loading | waypoints = Just value })
                        model.data
              }
            , Cmd.none
            )

        AddWaypointResponse { id, waypoint } ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints = Api.waypointIdKeyDict .insert id waypoint waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )

        DeleteWaypointResponse id {} ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints = Api.waypointIdKeyDict .remove id waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )


initData updateLoading data =
    case data of
        Loading loading ->
            loading
                |> updateLoading
                |> resolveData

        _ ->
            Error


resolveData loading =
    case ( loading.priorities, loading.waypoints ) of
        ( Just priorities, Just waypoints ) ->
            buildData
                { priorities = priorities
                , waypoints = waypoints
                }

        _ ->
            Loading loading


updateData fn remoteData =
    case remoteData of
        Loading _ ->
            Error

        Error ->
            remoteData

        Data data ->
            buildData (fn data)


buildData { priorities, waypoints } =
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
            |> Ui.append (Ui.loader (NetworkQueue.fold (always ((||) True)) False model.networkQueue))
            |> Ui.append
                (case model.data of
                    Data data ->
                        case model.screen of
                            Home ->
                                viewHome model data

                            WaypointDetail waypointId ->
                                viewDetail data waypointId

                            Oops reason ->
                                viewOops reason

                    Error ->
                        viewOops DataError

                    Loading _ ->
                        Ui.empty
                )
            |> Ui.toHtml
            |> List.map (Html.map UserAction)
    }


viewOops reason =
    Ui.heading Strings.oops
        |> Ui.append
            (case reason of
                UnknownPath ->
                    consts.strings.unknownPath
                        (\beginning link end ->
                            Ui.text beginning
                                |> Ui.append (Ui.link "/" (Just link))
                                |> Ui.append (Ui.text end)
                        )

                DataError ->
                    Ui.text consts.strings.dataError
            )


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


type WaypointListEntry a
    = Visible a
    | Hidden { total : Int }


viewHome model data =
    Ui.scaffold
        (viewWaypoints Nothing (globalFilter model.input) data)
        (Ui.input { text = model.input, onInput = ChangeInput }
            |> Ui.append
                (if model.input == "" then
                    Ui.empty

                 else
                    Ui.button ClickAddWaypoint consts.strings.add
                )
        )


viewDetail ({ waypoints } as data) waypointId =
    case Api.waypointIdKeyDict .get waypointId waypoints of
        Nothing ->
            viewOops UnknownPath

        Just { text } ->
            Ui.heading text
                |> Ui.append (Ui.button (ClickDeleteWaypoint waypointId) consts.strings.delete)
                |> Ui.append (viewWaypoints (Just waypointId) (always True) data)


viewWaypoints focusedWaypointId searchPredicate { priorities, sccNodeIds, graph, waypoints } =
    let
        seeds =
            graph
                |> Graph.nodes
                |> List.filter
                    (\n ->
                        Graph.nodes n.label
                            |> List.any (\{ label } -> focusedWaypointId == Just label)
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
                seeds
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
                seeds
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
                        |> List.map
                            (\id ->
                                let
                                    highlight =
                                        if focusedWaypointId == Just id then
                                            Just Ui.primary

                                        else if Graph.size scc > 1 then
                                            Just Ui.conflict

                                        else
                                            Nothing

                                    maybeWaypoint =
                                        Api.waypointIdKeyDict .get id waypoints
                                in
                                if focusedWaypointId == Nothing || Api.waypointIdKeyDict .member id transitiveRequires || Api.waypointIdKeyDict .member id transitiveRequiredBy then
                                    Just
                                        (if searchPredicate maybeWaypoint then
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

                                else
                                    Nothing
                            )
                )
            |> List.filterMap identity
            |> List.foldr
                (\item acc ->
                    case item of
                        Just content ->
                            Visible content :: acc

                        Nothing ->
                            case acc of
                                (Hidden { total }) :: rest ->
                                    Hidden { total = total + 1 } :: rest

                                _ ->
                                    Hidden { total = 1 } :: acc
                )
                []
            |> List.map
                (\item ->
                    case item of
                        Visible content ->
                            content

                        Hidden { total } ->
                            { bullet = Nothing
                            , highlight = Just Ui.diminished
                            , targetUrl = Nothing
                            , content =
                                Ui.text (consts.strings.skippedItems total)
                            }
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
    { bullet = Just icon
    , highlight = highlight
    , targetUrl = Just (Endpoint.joinPath [ "detail", Api.unwrapWaypointId id ])
    , content =
        Ui.text text
            |> Ui.append
                (case url of
                    Nothing ->
                        Ui.empty

                    Just justUrl ->
                        Ui.text " "
                            |> Ui.append (Ui.link justUrl Nothing)
                )
    }


makeNetworkRequest request ( model, previousCmd ) =
    let
        { queue, cmd } =
            NetworkQueue.enqueue requestToCmd (requestSafety request) request model.networkQueue
    in
    ( { model | networkQueue = queue }, Cmd.batch [ previousCmd, cmd ] )


requestToCmd token request =
    let
        tagWith tag result =
            NetworkResponse { token = token, result = Result.map tag result }
    in
    case request of
        AddWaypoint r ->
            Endpoint.request Api.addWaypoint r (tagWith AddWaypointResponse)

        DeleteWaypoint r ->
            Endpoint.request Api.deleteWaypoint r (tagWith (DeleteWaypointResponse r.id))

        InitWaypoints ->
            Endpoint.request Api.waypoints (tagWith InitWaypointsResponse)

        InitPriorities ->
            Endpoint.request Api.priorities (tagWith InitPrioritiesResponse)


requestSafety request =
    case request of
        AddWaypoint _ ->
            NetworkQueue.Unsafe

        DeleteWaypoint _ ->
            NetworkQueue.Idempotent

        InitWaypoints ->
            NetworkQueue.Safe

        InitPriorities ->
            NetworkQueue.Safe


subscriptions model =
    Sub.none


consts =
    { strings =
        { add = "+"
        , delete = "⨉"
        , skippedItems = \n -> "<" ++ String.fromInt n ++ " more>"
        , unknownPath = \combine -> combine "You wound up somewhere unexpected. " "Click here" " to go back home."
        , dataError = "Something unexpexted happened when loading your data."
        }
    }
