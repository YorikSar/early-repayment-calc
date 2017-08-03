module Main exposing (main)

import Css
import Date exposing (Date)
import Date.Extra
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
    , result : Maybe Float
    , intermediate_results : Maybe { r : Float, b : Float, x : Float, d1 : Float, d2 : Float, k1 : Float, k2 : Float, t : Float }
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
      , result = Nothing
      , intermediate_results = Nothing
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


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    case ( ma, mb, mc, md, me, mf ) of
        ( Just a, Just b, Just c, Just d, Just e, Just f ) ->
            Just (func a b c d e f)

        _ ->
            Nothing


calculate prev_date next_date early_date debt desired_sum rate =
    let
        r =
            rate / 36500

        b =
            debt

        x =
            desired_sum

        d1 =
            toFloat (Date.Extra.diff Date.Extra.Day prev_date early_date)

        d2 =
            toFloat (Date.Extra.diff Date.Extra.Day early_date next_date)

        k1 =
            d1 * r * b

        k2 =
            d2 * r * (b - x + k1) / (1 - d2 * r)

        t =
            x - k2
    in
        { result = t
        , intermediate_results =
            { r = r
            , b = b
            , x = x
            , d1 = d1
            , d2 = d2
            , k1 = k1
            , k2 = k2
            , t = t
            }
        }


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

        calcResult =
            maybeMap6 calculate model.prev_date model.next_date model.early_date model.debt model.desired_sum model.rate

        modelWithResult =
            case calcResult of
                Nothing ->
                    { newModel | result = Nothing, intermediate_results = Nothing }

                Just { result, intermediate_results } ->
                    { newModel | result = Just result, intermediate_results = Just intermediate_results }
    in
        ( modelWithResult, Cmd.none )


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
                , Html.text
                    (case model.result of
                        Just result ->
                            "You should pay: " ++ (toString result)

                        Nothing ->
                            ""
                    )
                , Html.pre []
                    [ Html.text
                        (case model.intermediate_results of
                            Just results ->
                                "R = "
                                    ++ (toString results.r)
                                    ++ "\n"
                                    ++ "B = "
                                    ++ (toString results.b)
                                    ++ "\n"
                                    ++ "X = "
                                    ++ (toString results.x)
                                    ++ "\n"
                                    ++ "d1 = "
                                    ++ (toString results.d1)
                                    ++ "\n"
                                    ++ "d2 = "
                                    ++ (toString results.d2)
                                    ++ "\n"
                                    ++ "K1 = "
                                    ++ (toString results.k1)
                                    ++ "\n"
                                    ++ "K2 = "
                                    ++ (toString results.k2)
                                    ++ "\n"
                                    ++ "T = "
                                    ++ (toString results.t)
                                    ++ "\n"

                            Nothing ->
                                ""
                        )
                    ]
                ]
            ]
