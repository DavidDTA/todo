module Frontend.Main exposing (main)

import Api
import Atlas
import Browser
import Browser.Navigation
import Build
import Css
import Dict
import Endpoint
import Frontend.Ports
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
import Result.Extra
import SortKey
import Strings
import Task
import Ui
import Url
import WaypointId


type RemoteData
    = Loading
        { waypoints : Maybe (KeyDict.KeyDict WaypointId.WaypointId String Api.Waypoint)
        }
    | Error OopsReason
    | Data
        { priorities : List { priority : String, waypointId : WaypointId.WaypointId }
        , waypoints : KeyDict.KeyDict WaypointId.WaypointId String Api.Waypoint
        , atlas : Atlas.Atlas
        }


type alias Model =
    { data : RemoteData
    , input : String
    , navigationKey : Browser.Navigation.Key
    , networkQueue : NetworkQueue.NetworkQueue NetworkRequest ()
    , screen : Maybe Screen
    }


type Screen
    = Home
    | WaypointDetail WaypointId.WaypointId


type Msg
    = DelayedInit
    | NetworkResponse { token : NetworkQueue.Token, result : Result.Result Http.Error NetworkResponse }
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | UserAction UserAction


type UserAction
    = ClickAddWaypoint
    | ClickCompleteWaypoint WaypointId.WaypointId
    | ClickDeleteWaypoint WaypointId.WaypointId
    | ChangeInput String
    | SelectWaypointPriority
        { call :
            { object : Json.Decode.Value
            , methodName : String
            , args : List Json.Decode.Value
            }
        , selection : Maybe { id : WaypointId.WaypointId, priority : Maybe String }
        }
    | SelectAddDependency
        { call :
            { object : Json.Decode.Value
            , methodName : String
            , args : List Json.Decode.Value
            }
        , selection : Maybe { from : WaypointId.WaypointId, to : WaypointId.WaypointId }
        }
    | SelectRemoveDependency
        { call :
            { object : Json.Decode.Value
            , methodName : String
            , args : List Json.Decode.Value
            }
        , selection : Maybe { from : WaypointId.WaypointId, to : WaypointId.WaypointId }
        }


type NetworkRequest
    = AddWaypoint { text : String }
    | AddDependency { from : WaypointId.WaypointId, to : WaypointId.WaypointId }
    | SetWaypointCompleted { id : WaypointId.WaypointId, completed : Bool }
    | SetWaypointPriority { id : WaypointId.WaypointId, priority : Maybe String }
    | DeleteWaypoint { id : WaypointId.WaypointId }
    | InitWaypoints
    | RemoveDependency { from : WaypointId.WaypointId, to : WaypointId.WaypointId }


type NetworkResponse
    = InitWaypointsResponse (List { id : WaypointId.WaypointId, waypoint : Api.Waypoint })
    | AddWaypointResponse { id : WaypointId.WaypointId, waypoint : Api.Waypoint }
    | AddDependencyResponse { from : WaypointId.WaypointId, to : WaypointId.WaypointId } {}
    | SetWaypointPriorityResponse { id : WaypointId.WaypointId, priority : Maybe String } {}
    | DeleteWaypointResponse WaypointId.WaypointId {}
    | SetWaypointCompletedResponse { id : WaypointId.WaypointId, completed : Bool } {}
    | RemoveDependencyResponse { from : WaypointId.WaypointId, to : WaypointId.WaypointId } {}


type OopsReason
    = UnknownPath
    | DataError
    | OutdatedApplication


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Json.Decode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        versionMatch =
            Json.Decode.decodeValue (Json.Decode.field "buildVersion" Json.Decode.string) flags
                |> Result.Extra.unwrap False ((==) Build.version)
    in
    ( { data =
            if versionMatch then
                Loading { waypoints = Nothing }

            else
                Error OutdatedApplication
      , input = ""
      , screen = parseScreen url.path
      , navigationKey = key
      , networkQueue = NetworkQueue.empty
      }
    , if versionMatch then
        Task.perform identity (Task.succeed DelayedInit)

      else
        Cmd.none
    )


parseScreen url =
    case Endpoint.splitPath url of
        Just [ "" ] ->
            Just Home

        Just [ "detail", id ] ->
            Just (WaypointDetail (WaypointId.WaypointId id))

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DelayedInit ->
            ( model, Cmd.none )
                |> makeNetworkRequest InitWaypoints

        NetworkResponse { token, result } ->
            case result of
                Err error ->
                    let
                        { queue, cmd } =
                            NetworkQueue.halt requestToCmd token () model.networkQueue
                    in
                    ( { model
                        | networkQueue = queue
                        , data =
                            case error of
                                Http.BadStatus 418 ->
                                    Error OutdatedApplication

                                _ ->
                                    Error DataError
                      }
                    , cmd
                    )

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

                ClickCompleteWaypoint id ->
                    let
                        wqypoint =
                            case model.data of
                                Loading _ ->
                                    Nothing

                                Error _ ->
                                    Nothing

                                Data { waypoints } ->
                                    Atlas.waypointIdKeyDict .get id waypoints

                        makeRequest =
                            case wqypoint of
                                Nothing ->
                                    identity

                                Just { completed } ->
                                    makeNetworkRequest (SetWaypointCompleted { id = id, completed = not completed })
                    in
                    ( model, Cmd.none )
                        |> makeRequest

                ClickDeleteWaypoint id ->
                    ( model
                    , Browser.Navigation.back model.navigationKey 1
                    )
                        |> makeNetworkRequest (DeleteWaypoint { id = id })

                SelectWaypointPriority { call, selection } ->
                    case selection of
                        Nothing ->
                            ( model, Frontend.Ports.callMethod call )

                        Just selection_ ->
                            ( model, Frontend.Ports.callMethod call )
                                |> makeNetworkRequest (SetWaypointPriority selection_)

                SelectAddDependency { call, selection } ->
                    case selection of
                        Nothing ->
                            ( model, Frontend.Ports.callMethod call )

                        Just selection_ ->
                            ( model, Frontend.Ports.callMethod call )
                                |> makeNetworkRequest (AddDependency selection_)

                SelectRemoveDependency { call, selection } ->
                    case selection of
                        Nothing ->
                            ( model, Frontend.Ports.callMethod call )

                        Just selection_ ->
                            ( model, Frontend.Ports.callMethod call )
                                |> makeNetworkRequest (RemoveDependency selection_)


updateForNetworkResponse response model =
    case response of
        InitWaypointsResponse value ->
            ( { model
                | data =
                    value
                        |> List.map (\{ id, waypoint } -> ( id, waypoint ))
                        |> Atlas.waypointIdKeyDict .fromList
                        |> (\keyDict ->
                                if Atlas.waypointIdKeyDict .size keyDict == List.length value then
                                    initData
                                        (\loading -> { loading | waypoints = Just keyDict })
                                        model.data

                                else
                                    Error DataError
                           )
              }
            , Cmd.none
            )

        AddWaypointResponse { id, waypoint } ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints = Atlas.waypointIdKeyDict .insert id waypoint waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )

        AddDependencyResponse { from, to } {} ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints =
                                Atlas.waypointIdKeyDict .update
                                    from
                                    (Maybe.map
                                        (\waypoint ->
                                            { waypoint
                                                | requires =
                                                    if List.member to waypoint.requires then
                                                        waypoint.requires

                                                    else
                                                        to :: waypoint.requires
                                            }
                                        )
                                    )
                                    waypoints
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
                            , waypoints = Atlas.waypointIdKeyDict .remove id waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )

        SetWaypointCompletedResponse { id, completed } {} ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints = Atlas.waypointIdKeyDict .update id (Maybe.map (\waypoint -> { waypoint | completed = completed })) waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )

        SetWaypointPriorityResponse { id, priority } {} ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints = Atlas.waypointIdKeyDict .update id (Maybe.map (\waypoint -> { waypoint | priority = priority })) waypoints
                            }
                        )
                        model.data
              }
            , Cmd.none
            )

        RemoveDependencyResponse { from, to } {} ->
            ( { model
                | data =
                    updateData
                        (\{ priorities, waypoints } ->
                            { priorities = priorities
                            , waypoints =
                                Atlas.waypointIdKeyDict .update
                                    from
                                    (Maybe.map
                                        (\waypoint ->
                                            { waypoint
                                                | requires =
                                                    List.filter ((/=) to) waypoint.requires
                                            }
                                        )
                                    )
                                    waypoints
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
            Error DataError


resolveData loading =
    case loading.waypoints of
        Just waypoints ->
            buildData
                { waypoints = waypoints
                }

        Nothing ->
            Loading loading


updateData fn remoteData =
    case remoteData of
        Loading _ ->
            Error DataError

        Error _ ->
            remoteData

        Data data ->
            buildData (fn data)


buildData { waypoints } =
    let
        priorities =
            waypoints
                |> Atlas.waypointIdKeyDict .toList
                |> List.filterMap
                    (\( id, waypoint ) ->
                        waypoint.priority
                            |> Maybe.map (\priority -> { waypointId = id, priority = priority })
                    )
                |> List.sortBy (\{ waypointId, priority } -> [ priority, (\(WaypointId.WaypointId id) -> id) waypointId ])

        atlas =
            Atlas.build
                (Atlas.waypointIdKeyDict .keys waypoints)
                (Atlas.waypointIdKeyDict .foldl
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
                    |> List.concatMap
                        (\edge ->
                            if Maybe.Extra.unwrap False .completed (Atlas.waypointIdKeyDict .get edge.from waypoints) then
                                [ edge, { from = edge.to, to = edge.from } ]

                            else
                                [ edge ]
                        )
                )
    in
    Data
        { atlas = atlas
        , priorities = priorities
        , waypoints = waypoints
        }


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title.main
    , body =
        Ui.global
            |> Ui.append (Ui.loader (NetworkQueue.fold (always ((||) True)) False model.networkQueue))
            |> Ui.append
                (case ( model.screen, model.data ) of
                    ( Just Home, Data data ) ->
                        viewHome model data

                    ( Just (WaypointDetail waypointId), Data data ) ->
                        viewDetail data waypointId

                    ( Nothing, _ ) ->
                        viewOops UnknownPath

                    ( _, Error reason ) ->
                        viewOops reason

                    ( _, Loading _ ) ->
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
                    consts.strings.unexpectedLocation
                        { normal = Ui.text
                        , link = Just >> Ui.link "/"
                        }
                        |> Ui.concat

                DataError ->
                    Ui.text consts.strings.dataError

                OutdatedApplication ->
                    Ui.text consts.strings.outdatedApplication
            )


globalFilter input waypoints id =
    let
        words =
            String.words input
                |> List.map String.toLower

        text =
            Atlas.waypointIdKeyDict .get id waypoints
                |> Maybe.map .text
                |> Maybe.withDefault ""
                |> String.toLower

        url =
            Atlas.waypointIdKeyDict .get id waypoints
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
        (if model.input == "" then
            let
                entireGroupCompleted =
                    Atlas.waypointGroups data.atlas
                        |> List.filter
                            (\group ->
                                Atlas.waypointIdKeyDict .foldl (\waypointId {} acc -> acc && Maybe.Extra.unwrap False .completed (Atlas.waypointIdKeyDict .get waypointId data.waypoints)) True group
                            )
                        |> List.foldl (Atlas.waypointIdKeyDict .union) (Atlas.waypointIdKeyDict .empty)
            in
            viewWaypoints (\id -> not (Atlas.waypointIdKeyDict .member id entireGroupCompleted)) data
                |> Ui.append
                    (viewWaypoints (\id -> Atlas.waypointIdKeyDict .member id entireGroupCompleted) data)

         else
            viewWaypoints (globalFilter model.input data.waypoints) data
        )
        (Ui.input { text = model.input, onInput = ChangeInput }
            |> Ui.append
                (if model.input == "" then
                    Ui.empty

                 else
                    Ui.button ClickAddWaypoint consts.strings.add
                )
        )


viewDetail ({ waypoints, priorities } as data) waypointId =
    case Atlas.waypointIdKeyDict .get waypointId waypoints of
        Nothing ->
            viewOops UnknownPath

        Just { text, completed } ->
            let
                prioritiesFilteredForCompletion =
                    List.filter
                        (\priority ->
                            completed
                                || (Atlas.waypointIdKeyDict .get priority.waypointId waypoints
                                        |> Maybe.Extra.unwrap True (.completed >> not)
                                   )
                        )
                        priorities

                priorityIndex =
                    List.Extra.findIndex (.waypointId >> (==) waypointId) prioritiesFilteredForCompletion

                prioritiesFilteredWithThisRemoved =
                    List.filter (.waypointId >> (/=) waypointId) prioritiesFilteredForCompletion

                incomingDirect =
                    Atlas.incoming waypointId data.atlas

                incomingIndirect =
                    Atlas.waypointIdKeyDict .diff
                        (Atlas.transitiveIncoming waypointId data.atlas)
                        incomingDirect
                        |> Atlas.waypointIdKeyDict .remove waypointId

                outgoingDirect =
                    Atlas.outgoing waypointId data.atlas

                outgoingIndirect =
                    Atlas.waypointIdKeyDict .diff
                        (Atlas.transitiveOutgoing waypointId data.atlas)
                        outgoingDirect
                        |> Atlas.waypointIdKeyDict .remove waypointId
            in
            Ui.heading text
                |> Ui.append (Ui.button (ClickDeleteWaypoint waypointId) consts.strings.delete)
                |> Ui.append
                    (Ui.button (ClickCompleteWaypoint waypointId)
                        (if completed then
                            consts.strings.complete

                         else
                            consts.strings.incomplete
                        )
                    )
                |> Ui.append
                    (Ui.select
                        (prioritiesFilteredWithThisRemoved
                            |> List.map
                                (\priority ->
                                    { text =
                                        Atlas.waypointIdKeyDict .get priority.waypointId waypoints
                                            |> Maybe.map .text
                                            |> Maybe.withDefault Strings.unknownWaypoint
                                    , selected = False
                                    , msg = Nothing
                                    }
                                )
                            |> List.Extra.interweave
                                (List.map3
                                    (\before after index ->
                                        { text =
                                            if Just index == priorityIndex then
                                                text

                                            else
                                                ""
                                        , selected = False
                                        , msg =
                                            Just
                                                { id = waypointId
                                                , priority =
                                                    if Just index == priorityIndex then
                                                        Nothing

                                                    else
                                                        Just
                                                            (case ( before, after ) of
                                                                ( Nothing, Nothing ) ->
                                                                    SortKey.init

                                                                ( Nothing, Just after_ ) ->
                                                                    SortKey.before after_.priority

                                                                ( Just before_, Nothing ) ->
                                                                    SortKey.after before_.priority

                                                                ( Just before_, Just after_ ) ->
                                                                    SortKey.between before_.priority after_.priority
                                                            )
                                                }
                                        }
                                    )
                                    ([ Nothing ] ++ List.map Just prioritiesFilteredWithThisRemoved)
                                    (List.map Just prioritiesFilteredWithThisRemoved ++ [ Nothing ])
                                    (List.range 0 (List.length prioritiesFilteredWithThisRemoved))
                                )
                            |> (::)
                                { text =
                                    case priorityIndex of
                                        Nothing ->
                                            consts.strings.unprioritized

                                        Just priorityIndex_ ->
                                            consts.strings.prioritized priorityIndex_
                                , selected = True
                                , msg = Nothing
                                }
                        )
                        SelectWaypointPriority
                    )
                |> Ui.append (Ui.heading consts.strings.dependenciesOutgoingIndirect)
                |> Ui.append
                    (viewWaypoints (\candidate -> Atlas.waypointIdKeyDict .member candidate outgoingIndirect) data)
                |> Ui.append (Ui.heading consts.strings.dependenciesOutgoingDirect)
                |> Ui.append
                    (viewWaypoints (\candidate -> Atlas.waypointIdKeyDict .member candidate outgoingDirect) data)
                |> Ui.append
                    (Ui.select
                        (data.waypoints
                            |> Atlas.waypointIdKeyDict .toList
                            |> List.map
                                (\( key, value ) ->
                                    { msg = Just { from = waypointId, to = key }
                                    , selected = False
                                    , text = value.text
                                    }
                                )
                            |> (::)
                                { msg = Nothing
                                , selected = True
                                , text = "+"
                                }
                        )
                        SelectAddDependency
                    )
                |> Ui.append
                    (Ui.select
                        (data.waypoints
                            |> Atlas.waypointIdKeyDict .filter
                                (\candidate _ -> Atlas.waypointIdKeyDict .member candidate outgoingDirect)
                            |> Atlas.waypointIdKeyDict .toList
                            |> List.map
                                (\( key, value ) ->
                                    { msg = Just { from = waypointId, to = key }
                                    , selected = False
                                    , text = value.text
                                    }
                                )
                            |> (::)
                                { msg = Nothing
                                , selected = True
                                , text = "-"
                                }
                        )
                        SelectRemoveDependency
                    )
                |> Ui.append (Ui.heading consts.strings.dependenciesIncomingDirect)
                |> Ui.append
                    (viewWaypoints (\candidate -> Atlas.waypointIdKeyDict .member candidate incomingDirect) data)
                |> Ui.append
                    (Ui.select
                        (data.waypoints
                            |> Atlas.waypointIdKeyDict .toList
                            |> List.map
                                (\( key, value ) ->
                                    { msg = Just { from = key, to = waypointId }
                                    , selected = False
                                    , text = value.text
                                    }
                                )
                            |> (::)
                                { msg = Nothing
                                , selected = True
                                , text = "+"
                                }
                        )
                        SelectAddDependency
                    )
                |> Ui.append
                    (Ui.select
                        (data.waypoints
                            |> Atlas.waypointIdKeyDict .filter
                                (\candidate _ -> Atlas.waypointIdKeyDict .member candidate incomingDirect)
                            |> Atlas.waypointIdKeyDict .toList
                            |> List.map
                                (\( key, value ) ->
                                    { msg = Just { from = key, to = waypointId }
                                    , selected = False
                                    , text = value.text
                                    }
                                )
                            |> (::)
                                { msg = Nothing
                                , selected = True
                                , text = "-"
                                }
                        )
                        SelectRemoveDependency
                    )
                |> Ui.append (Ui.heading consts.strings.dependenciesIncomingIndirect)
                |> Ui.append
                    (viewWaypoints (\candidate -> Atlas.waypointIdKeyDict .member candidate incomingIndirect) data)


viewWaypoints filter { priorities, atlas, waypoints } =
    Ui.list
        (squeeze priorities atlas
            |> List.concatMap
                (\group ->
                    group
                        |> Atlas.waypointIdKeyDict .keys
                        |> List.filterMap
                            (\id ->
                                let
                                    highlight =
                                        if Atlas.waypointIdKeyDict .size group > 1 then
                                            Just Ui.conflict

                                        else
                                            Nothing

                                    maybeWaypoint =
                                        Atlas.waypointIdKeyDict .get id waypoints
                                in
                                if filter id then
                                    Just
                                        (case maybeWaypoint of
                                            Just waypoint ->
                                                viewWaypointRow
                                                    { text = waypoint.text
                                                    , completed = waypoint.completed
                                                    , highlight = highlight
                                                    , id = id
                                                    , priority = Maybe.Extra.isJust waypoint.priority
                                                    , url = waypoint.url
                                                    }

                                            Nothing ->
                                                viewWaypointRowPrimitive
                                                    { text = Strings.unknownWaypoint
                                                    , starred = False
                                                    , id = id
                                                    , highlight = highlight
                                                    , strikethrough = False
                                                    , url = Nothing
                                                    }
                                        )

                                else
                                    Nothing
                            )
                )
        )


squeeze priorities atlas =
    let
        waypointPriorities =
            priorities
                |> List.indexedMap
                    (\priorityIndex priority ->
                        { transitiveWaypointIds =
                            Atlas.transitiveOutgoing
                                priority.waypointId
                                atlas
                        , priorityIndex = priorityIndex
                        }
                    )
                |> List.foldr
                    (\{ priorityIndex, transitiveWaypointIds } acc ->
                        Atlas.waypointIdKeyDict .foldl
                            (\waypointId {} ->
                                Atlas.waypointIdKeyDict .update
                                    waypointId
                                    (Maybe.withDefault [] >> (::) (List.length priorities - priorityIndex) >> Just)
                            )
                            acc
                            transitiveWaypointIds
                    )
                    (Atlas.waypointIdKeyDict .empty)

        externalDirectIncoming =
            externalDirectHelp Atlas.incoming

        externalDirectOutgoing =
            externalDirectHelp Atlas.outgoing

        externalDirectHelp direction waypointIds =
            Atlas.waypointIdKeyDict .foldl
                (\waypointId {} acc ->
                    Atlas.waypointIdKeyDict .foldl
                        (\neighborWaypointId {} -> Atlas.waypointIdKeyDict .insert neighborWaypointId {})
                        acc
                        (direction waypointId atlas)
                )
                (Atlas.waypointIdKeyDict .empty)
                waypointIds
                |> Atlas.waypointIdKeyDict .filter (\candidate {} -> not (Atlas.waypointIdKeyDict .member candidate waypointIds))

        initialQueue =
            Atlas.waypointGroups atlas
                |> List.foldl
                    (\group acc ->
                        if Atlas.waypointIdKeyDict .isEmpty (externalDirectOutgoing group) then
                            Atlas.waypointIdKeyDict .foldl (\groupMember {} -> Heap.push groupMember) acc group

                        else
                            acc
                    )
                    (Heap.empty
                        (Heap.biggest
                            |> Heap.by
                                (\waypointId ->
                                    Atlas.waypointIdKeyDict .get waypointId waypointPriorities |> Maybe.withDefault []
                                )
                        )
                    )

        step queue selected stepAcc =
            case Heap.pop queue of
                Nothing ->
                    stepAcc

                Just ( head, tail ) ->
                    if Atlas.waypointIdKeyDict .member head selected then
                        step tail selected stepAcc

                    else
                        let
                            waypointGroup =
                                Atlas.waypointGroup head atlas

                            updatedSelected =
                                Atlas.waypointIdKeyDict .foldl (\waypointId {} -> Atlas.waypointIdKeyDict .insert waypointId {}) selected waypointGroup

                            incoming =
                                externalDirectIncoming waypointGroup

                            updatedQueue =
                                Atlas.waypointIdKeyDict .foldl
                                    (\incomingWaypointId {} acc ->
                                        if Atlas.waypointIdKeyDict .isEmpty (Atlas.waypointIdKeyDict .diff (Atlas.outgoing incomingWaypointId atlas) updatedSelected) then
                                            Heap.push incomingWaypointId acc

                                        else
                                            acc
                                    )
                                    tail
                                    incoming
                        in
                        step updatedQueue updatedSelected (waypointGroup :: stepAcc)
    in
    step initialQueue (Atlas.waypointIdKeyDict .empty) []
        |> List.reverse


viewWaypointRow { completed, highlight, id, priority, text, url } =
    viewWaypointRowPrimitive
        { text = text
        , starred = priority
        , id = id
        , highlight = highlight
        , strikethrough = completed
        , url = url
        }


viewWaypointRowPrimitive { highlight, starred, id, strikethrough, text, url } =
    { starred = starred
    , highlight = highlight
    , strikethrough = strikethrough
    , targetUrl = Just (Endpoint.joinPath [ "detail", (\(WaypointId.WaypointId id_) -> id_) id ])
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
            Endpoint.request Api.waypointAdd r (tagWith AddWaypointResponse)

        AddDependency r ->
            Endpoint.request Api.waypointAddDependency r (tagWith (AddDependencyResponse r))

        SetWaypointCompleted r ->
            Endpoint.request Api.waypointSetCompleted r (tagWith (SetWaypointCompletedResponse r))

        SetWaypointPriority r ->
            Endpoint.request Api.waypointSetPriority r (tagWith (SetWaypointPriorityResponse r))

        DeleteWaypoint r ->
            Endpoint.request Api.waypointDelete r (tagWith (DeleteWaypointResponse r.id))

        InitWaypoints ->
            Endpoint.request Api.waypoints (tagWith InitWaypointsResponse)

        RemoveDependency r ->
            Endpoint.request Api.waypointRemoveDependency r (tagWith (RemoveDependencyResponse r))


requestSafety request =
    case request of
        AddWaypoint _ ->
            NetworkQueue.Unsafe

        AddDependency _ ->
            NetworkQueue.Idempotent

        SetWaypointCompleted _ ->
            NetworkQueue.Idempotent

        SetWaypointPriority _ ->
            NetworkQueue.Idempotent

        DeleteWaypoint _ ->
            NetworkQueue.Idempotent

        InitWaypoints ->
            NetworkQueue.Safe

        RemoveDependency _ ->
            NetworkQueue.Idempotent


subscriptions model =
    Sub.none


consts =
    { strings =
        { add = "+"
        , delete = "⨉"
        , complete = "☑"
        , incomplete = "☐"
        , dependenciesIncomingDirect = "Incoming Direct Dependencies"
        , dependenciesIncomingIndirect = "Incoming Indirect Dependencies"
        , dependenciesOutgoingDirect = "Outgoing Direct Dependencies"
        , dependenciesOutgoingIndirect = "Outgoing Indirect Dependencies"
        , outdatedApplication = "Application is outdated. Please refresh."
        , requires = "Requirements"
        , requiredByDirect = "Required By"
        , requiredByIndirect = "Required By"
        , skippedItems = \n -> "<" ++ String.fromInt n ++ " more>"
        , unexpectedLocation =
            \{ normal, link } ->
                [ normal "You wound up somewhere unexpected. "
                , link "Click here"
                , normal " to go back home."
                ]
        , dataError = "Something unexpexted happened when loading your data."
        , prioritized = \index -> "Priority: " ++ String.fromInt index
        , unprioritized = "Unprioritized"
        }
    }
