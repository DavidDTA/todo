module Ui exposing
    ( append
    , button
    , concat
    , conflict
    , diminished
    , empty
    , global
    , heading
    , input
    , link
    , list
    , loader
    , primary
    , scaffold
    , select
    , text
    , toHtml
    )

import Css
import Css.Global
import Dict
import Html.Events.Extra.Pointer
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import List.Extra


type Flow msg
    = Flow (List (Html.Styled.Html msg))


type Highlight
    = Primary
    | Secondary
    | Conflict
    | Diminished


toHtml (Flow v) =
    List.map Html.Styled.toUnstyled v


unwrap (Flow v) =
    v


primary =
    Primary


conflict =
    Conflict


diminished =
    Diminished


empty =
    Flow []


append (Flow back) (Flow front) =
    Flow (front ++ back)


concat flows =
    Flow (List.concatMap (\(Flow flow) -> flow) flows)


heading text_ =
    Flow [ Html.Styled.div [] [ Html.Styled.text text_ ] ]


list items =
    items
        |> List.map
            (\{ content, highlight, starred, strikethrough, targetUrl } ->
                let
                    container =
                        case targetUrl of
                            Nothing ->
                                identity

                            Just url ->
                                Html.Styled.a
                                    [ Html.Styled.Attributes.href url
                                    , Html.Styled.Attributes.css
                                        [ Css.display Css.block
                                        ]
                                    ]
                                    >> List.singleton
                in
                Html.Styled.li
                    ([ Html.Styled.Attributes.css
                        [ Css.listStyleType
                            (if starred then
                                Css.string "☆ "

                             else
                                Css.none
                            )
                        , Css.backgroundColor
                            (case highlight of
                                Nothing ->
                                    Css.unset

                                Just Primary ->
                                    Css.rgb 255 255 128

                                Just Secondary ->
                                    Css.rgb 160 160 255

                                Just Conflict ->
                                    Css.rgb 255 160 160

                                Just Diminished ->
                                    Css.rgb 192 192 192
                            )
                        , Css.textDecoration
                            (if strikethrough then
                                Css.lineThrough

                             else
                                Css.none
                            )
                        ]
                        |> Just
                     ]
                        |> List.filterMap identity
                    )
                    (unwrap content |> container)
            )
        |> Html.Styled.ul []
        |> List.singleton
        |> Flow


select items tag =
    let
        msgDict =
            items
                |> List.Extra.indexedFoldl
                    (\index item acc ->
                        case item.msg of
                            Nothing ->
                                acc

                            Just msg ->
                                Dict.insert index msg acc
                    )
                    Dict.empty

        decoder =
            Json.Decode.map2
                (\form selection ->
                    { call =
                        { object = form
                        , methodName = "reset"
                        , args = []
                        }
                    , selection = selection
                    }
                )
                (Json.Decode.at [ "target", "form" ] Json.Decode.value)
                (Json.Decode.at [ "target", "selectedIndex" ] Json.Decode.int
                    |> Json.Decode.map (\index -> Dict.get index msgDict)
                )
                |> Json.Decode.map tag
    in
    items
        |> List.map
            (\item ->
                Html.Styled.option
                    ([ if item.msg == Nothing then
                        [ Html.Styled.Attributes.attribute "disabled" "" ]

                       else
                        []
                     , if item.selected then
                        [ Html.Styled.Attributes.attribute "selected" "" ]

                       else
                        []
                     ]
                        |> List.concat
                    )
                    [ Html.Styled.text item.text ]
            )
        |> Html.Styled.select
            [ Html.Styled.Events.on "input" decoder
            ]
        |> List.singleton
        |> Html.Styled.form []
        |> List.singleton
        |> Flow


text text_ =
    Html.Styled.text text_ |> List.singleton |> Flow


link url text_ =
    Html.Styled.a
        [ Html.Styled.Attributes.href url
        ]
        [ Html.Styled.text (Maybe.withDefault "🔗" text_)
        ]
        |> List.singleton
        |> Flow


global =
    Flow
        [ Css.Global.global
            [ Css.Global.body
                [ Css.height (Css.pct 100) ]
            , Css.Global.html
                [ Css.height (Css.pct 100) ]
            ]
        ]


loader isLoading =
    Flow
        [ Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.position Css.absolute
                , Css.top Css.zero
                , Css.right Css.zero
                , Css.overflow Css.clip
                ]
            ]
            [ Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.px 16)
                    , Css.height (Css.px 16)
                    , Css.margin (Css.px 8)
                    , Css.backgroundColor (Css.rgb 0 0 0)
                    , Css.transform
                        (if isLoading then
                            Css.translate2 Css.zero Css.zero

                         else
                            Css.translate2 (Css.px 32) (Css.px -32)
                        )
                    , Css.property "transition" "transform 0.42s cubic-bezier(0.5, -0.5, 0.5, -0.5)"
                    ]
                ]
                []
            ]
        ]


scaffold (Flow main) (Flow bottom) =
    Flow
        [ Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.height (Css.pct 100)
                , Css.display Css.flex_
                , Css.flexDirection Css.column
                , Css.width (Css.pct 100)
                ]
            ]
            [ Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.flexGrow (Css.num 1)
                    , Css.flexBasis Css.zero
                    , Css.overflow Css.auto
                    ]
                ]
                main
            , Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.height (Css.px 48)
                    ]
                ]
                bottom
            ]
        ]


input params =
    Flow
        [ Html.Styled.input
            [ Html.Styled.Attributes.value params.text
            , Html.Styled.Events.onInput params.onInput
            ]
            []
        ]


button action text_ =
    Html.Styled.form
        [ Html.Styled.Events.onSubmit action
        ]
        [ Html.Styled.button
            []
            [ Html.Styled.text text_
            ]
        ]
        |> List.singleton
        |> Flow
