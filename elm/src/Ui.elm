module Ui exposing
    ( alert
    , append
    , conflict
    , empty
    , global
    , heading
    , input
    , link
    , list
    , primary
    , scaffold
    , secondary
    , text
    , toHtml
    )

import Css
import Css.Global
import Html.Events.Extra.Pointer
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events


type Flow msg
    = Flow (List (Html.Styled.Html msg))


type Highlight
    = Primary
    | Secondary
    | Conflict


toHtml (Flow v) =
    List.map Html.Styled.toUnstyled v


unwrap (Flow v) =
    v


primary =
    Primary


secondary =
    Secondary


conflict =
    Conflict


empty =
    Flow []


append (Flow back) (Flow front) =
    Flow (front ++ back)


alert text_ =
    Flow [ Html.Styled.text text_ ]


heading text_ =
    Flow [ Html.Styled.div [] [ Html.Styled.text text_ ] ]


list items =
    items
        |> List.map
            (\{ content, highlight, bullet, onEnter, onExit } ->
                Html.Styled.li
                    [ Html.Styled.Attributes.css
                        [ Css.listStyleType
                            (Css.string (bullet ++ " "))
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
                            )
                        ]
                    , Html.Events.Extra.Pointer.onEnter (always onEnter)
                        |> Html.Styled.Attributes.fromUnstyled
                    , Html.Events.Extra.Pointer.onLeave (always onExit)
                        |> Html.Styled.Attributes.fromUnstyled
                    ]
                    (unwrap content)
            )
        |> Html.Styled.ul []
        |> List.singleton
        |> Flow


text text_ =
    Html.Styled.text text_ |> List.singleton |> Flow


link url =
    Html.Styled.a
        [ Html.Styled.Attributes.href url
        ]
        [ Html.Styled.text "🔗"
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


input { onInput } =
    Flow
        [ Html.Styled.input
            [ Html.Styled.Events.onInput onInput
            ]
            []
        ]
