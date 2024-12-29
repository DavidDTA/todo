module Login exposing (main)

import Browser
import Browser.Navigation
import Form
import Form.Field
import Form.FieldView
import Form.Validation
import Html
import Html.Attributes
import Http
import Json.Encode
import Strings


type SubmissionState
    = Idle
    | InFlight


type alias Model =
    { formState : Form.Model
    , submissionState : SubmissionState
    }


type Msg
    = FormMsg (Form.Msg Msg)
    | FormSubmit (Form.Validated () String)
    | FormResponse (Result Http.Error ())


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { formState = Form.init
      , submissionState = Idle
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg formMsg ->
            let
                ( formState, cmd ) =
                    Form.update formMsg model.formState
            in
            ( { model
                | formState = formState
              }
            , cmd
            )

        FormSubmit parsed ->
            case ( parsed, model.submissionState ) of
                ( Form.Valid token, Idle ) ->
                    ( { model
                        | submissionState = InFlight
                      }
                    , Http.post
                        { url = "/-/api/login"
                        , body =
                            Http.jsonBody
                                (Json.Encode.object
                                    [ ( "token", Json.Encode.string token )
                                    ]
                                )
                        , expect = Http.expectWhatever FormResponse
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        FormResponse result ->
            case result of
                Ok _ ->
                    ( model, Browser.Navigation.reload )

                Err _ ->
                    ( { model
                        | submissionState = Idle
                      }
                    , Cmd.none
                    )


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title.login
    , body =
        [ Form.form
            (\token ->
                { combine = token
                , view =
                    \formState ->
                        [ Form.FieldView.input [] token
                        , Html.button
                            [ Html.Attributes.disabled formState.submitting
                            ]
                            [ Html.text Strings.logIn
                            ]
                        ]
                }
            )
            |> Form.field "token" (Form.Field.text |> Form.Field.required ())
            |> Form.renderHtml
                { submitting =
                    case model.submissionState of
                        Idle ->
                            False

                        InFlight ->
                            True
                , state = model.formState
                , toMsg = FormMsg
                }
                (Form.options formId
                    |> Form.withOnSubmit (.parsed >> FormSubmit)
                )
                []
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


formId =
    "form"
