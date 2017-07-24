module Main exposing (main)

import Css
import Date exposing (Date)
import DateTimePicker
import DateTimePicker.Css
import Html exposing (Html)
import Html.CssHelpers


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { prev_date : Maybe Date
    , prev_date_state : DateTimePicker.State
    , next_date : Maybe Date
    , next_date_state : DateTimePicker.State
    }


init : ( Model, Cmd Msg )
init =
    ( { prev_date = Nothing
      , prev_date_state = DateTimePicker.initialState
      , next_date = Nothing
      , next_date_state = DateTimePicker.initialState
      }
    , Cmd.batch
        [ DateTimePicker.initialCmd PrevDateChange DateTimePicker.initialState
        , DateTimePicker.initialCmd NextDateChange DateTimePicker.initialState
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PrevDateChange DateTimePicker.State (Maybe Date)
    | NextDateChange DateTimePicker.State (Maybe Date)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrevDateChange datePickerState selectedDate ->
            ( { model | prev_date = selectedDate, prev_date_state = datePickerState }, Cmd.none )

        NextDateChange datePickerState selectedDate ->
            ( { model | next_date = selectedDate, next_date_state = datePickerState }, Cmd.none )


{ id, class, classList } =
    Html.CssHelpers.withNamespace ""


view : Model -> Html Msg
view model =
    let
        { css } =
            Css.compile [ DateTimePicker.Css.css ]
    in
        Html.form []
            [ Html.node "style" [] [ Html.text css ]
            , Html.div []
                [ Html.label []
                    [ Html.text "Previous payment date:"
                    , DateTimePicker.datePicker PrevDateChange [] model.prev_date_state model.prev_date
                    ]
                , Html.label []
                    [ Html.text "Next payment date:"
                    , DateTimePicker.datePicker NextDateChange [] model.next_date_state model.next_date
                    ]
                ]
            ]
