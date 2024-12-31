module Main exposing (main)

import Browser
import Css
import Dict
import Html.Styled
import Html.Styled.Attributes
import Http
import Json.Decode
import KeyDict
import Strings


type WaypointId
    = WaypointId String


type alias Waypoint =
    { text : String
    , completed : Bool
    }


type RemoteData a
    = Loading
    | Error
    | Data a


type alias Model =
    { waypoints : RemoteData (KeyDict.KeyDict WaypointId String Waypoint)
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

                        Ok value ->
                            Data value
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
                [ Html.Styled.ul
                    []
                    (waypoints
                        |> waypointIdKeyDict .values
                        |> List.map
                            (\{ text, completed } ->
                                Html.Styled.li
                                    [ Html.Styled.Attributes.css
                                        [ Css.listStyleType
                                            (Css.string
                                                (if completed then
                                                    "☑ "

                                                 else
                                                    "☐ "
                                                )
                                            )
                                        ]
                                    ]
                                    [ Html.Styled.text text
                                    ]
                            )
                    )
                ]
        )
            |> List.map Html.Styled.toUnstyled
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


decodeWaypoints =
    Json.Decode.field
        "waypoints"
        (Json.Decode.list
            (Json.Decode.map3
                (\id text completed -> ( WaypointId id, Waypoint text completed ))
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "text" Json.Decode.string)
                (Json.Decode.field "completed" Json.Decode.bool)
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
