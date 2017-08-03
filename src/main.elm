module Main exposing (main)

import Css
import Date exposing (Date)
import DateTimePicker
import DateTimePicker.Css
import Html exposing (Html)
import Html.CssHelpers
import Html.Events


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { prev_date : Maybe Date
    , prev_date_state : DateTimePicker.State
    , next_date : Maybe Date
    , next_date_state : DateTimePicker.State
    , early_date : Maybe Date
    , early_date_state : DateTimePicker.State
    , debt : Maybe Float
    , desired_sum : Maybe Float
    , rate : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( { prev_date = Nothing
      , prev_date_state = DateTimePicker.initialState
      , next_date = Nothing
      , next_date_state = DateTimePicker.initialState
      , early_date = Nothing
      , early_date_state = DateTimePicker.initialState
      , debt = Nothing
      , desired_sum = Nothing
      , rate = Nothing
      }
    , Cmd.batch
        [ DateTimePicker.initialCmd PrevDateChange DateTimePicker.initialState
        , DateTimePicker.initialCmd NextDateChange DateTimePicker.initialState
        , DateTimePicker.initialCmd EarlyDateChange DateTimePicker.initialState
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PrevDateChange DateTimePicker.State (Maybe Date)
    | NextDateChange DateTimePicker.State (Maybe Date)
    | EarlyDateChange DateTimePicker.State (Maybe Date)
    | DebtChange String
    | DesiredSumChange String
    | RateChange String


stringToMaybeFloat str =
    case String.toFloat str of
        Ok value ->
            Just value

        Err _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                PrevDateChange datePickerState selectedDate ->
                    { model | prev_date = selectedDate, prev_date_state = datePickerState }

                NextDateChange datePickerState selectedDate ->
                    { model | next_date = selectedDate, next_date_state = datePickerState }

                EarlyDateChange datePickerState selectedDate ->
                    { model | early_date = selectedDate, early_date_state = datePickerState }

                DebtChange debtStr ->
                    { model | debt = stringToMaybeFloat debtStr }

                DesiredSumChange desiredSumStr ->
                    { model | desired_sum = stringToMaybeFloat desiredSumStr }

                RateChange rateStr ->
                    { model | rate = stringToMaybeFloat rateStr }
    in
        ( newModel, Cmd.none )


{ id, class, classList } =
    Html.CssHelpers.withNamespace ""


plainInput event =
    Html.div [] [ Html.input [ Html.Events.onInput event ] [] ]


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
                    [ Html.text "Loan body:"
                    , plainInput DebtChange
                    ]
                , Html.br [] []
                , Html.label []
                    [ Html.text "Yearly rate:"
                    , plainInput RateChange
                    ]
                , Html.br [] []
                , Html.label []
                    [ Html.text "Previous payment date:"
                    , DateTimePicker.datePicker PrevDateChange [] model.prev_date_state model.prev_date
                    ]
                , Html.label []
                    [ Html.text "Next payment date:"
                    , DateTimePicker.datePicker NextDateChange [] model.next_date_state model.next_date
                    ]
                , Html.label []
                    [ Html.text "Desired early payment date:"
                    , DateTimePicker.datePicker EarlyDateChange [] model.early_date_state model.early_date
                    ]
                , Html.label []
                    [ Html.text "Desired total payment in this month:"
                    , plainInput DesiredSumChange
                    ]
                ]
            ]
