module Main exposing (calculate, main)

import Browser
import Date exposing (Date)
import DatePicker
import Html exposing (Html)
import Html.Events
import Maybe


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { prev_date : Maybe Date
    , prev_date_state : DatePicker.DatePicker
    , next_date : Maybe Date
    , next_date_state : DatePicker.DatePicker
    , early_date : Maybe Date
    , early_date_state : DatePicker.DatePicker
    , debt : Maybe Float
    , desired_sum : Maybe Float
    , rate : Maybe Float
    , result : Maybe Float
    , intermediate_results : Maybe { r : Float, b : Float, x : Float, d1 : Float, d2 : Float, k1 : Float, k2 : Float, t : Float }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( prev_date_state, prev_date_init_cmd ) =
            DatePicker.init

        ( next_date_state, next_date_init_cmd ) =
            DatePicker.init

        ( early_date_state, early_date_init_cmd ) =
            DatePicker.init
    in
    ( { prev_date = Nothing
      , prev_date_state = prev_date_state
      , next_date = Nothing
      , next_date_state = next_date_state
      , early_date = Nothing
      , early_date_state = early_date_state
      , debt = Nothing
      , desired_sum = Nothing
      , rate = Nothing
      , result = Nothing
      , intermediate_results = Nothing
      }
    , Cmd.batch
        [ Cmd.map PrevDateMsg prev_date_init_cmd
        , Cmd.map NextDateMsg next_date_init_cmd
        , Cmd.map EarlyDateMsg early_date_init_cmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PrevDateMsg DatePicker.Msg
    | NextDateMsg DatePicker.Msg
    | EarlyDateMsg DatePicker.Msg
    | DebtChange String
    | DesiredSumChange String
    | RateChange String


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    Maybe.andThen (\a -> Maybe.map5 (func a) mb mc md me mf) ma


calculate prev_date next_date early_date debt desired_sum rate =
    let
        r =
            rate / 36500

        b =
            debt

        x =
            desired_sum

        d1 =
            toFloat (Date.diff Date.Days prev_date early_date)

        d2 =
            toFloat (Date.diff Date.Days early_date next_date)

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


handlePicker : DatePicker.Msg -> Maybe Date -> DatePicker.DatePicker -> ( DatePicker.DatePicker, Maybe Date )
handlePicker msg date state =
    let
        ( newState, dateEvent ) =
            DatePicker.update DatePicker.defaultSettings msg state
    in
    ( newState
    , case dateEvent of
        DatePicker.Picked newDate ->
            Just newDate

        _ ->
            date
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                PrevDateMsg dateMsg ->
                    let
                        ( state, date ) =
                            handlePicker dateMsg model.prev_date model.prev_date_state
                    in
                    { model | prev_date = date, prev_date_state = state }

                NextDateMsg dateMsg ->
                    let
                        ( state, date ) =
                            handlePicker dateMsg model.next_date model.next_date_state
                    in
                    { model | next_date = date, next_date_state = state }

                EarlyDateMsg dateMsg ->
                    let
                        ( state, date ) =
                            handlePicker dateMsg model.early_date model.early_date_state
                    in
                    { model | early_date = date, early_date_state = state }

                DebtChange debtStr ->
                    { model | debt = String.toFloat debtStr }

                DesiredSumChange desiredSumStr ->
                    { model | desired_sum = String.toFloat desiredSumStr }

                RateChange rateStr ->
                    { model | rate = String.toFloat rateStr }

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


plainInput event =
    Html.div [] [ Html.input [ Html.Events.onInput event ] [] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Early Repayment Calc"
    , body =
        [ Html.form []
            [ Html.div []
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
                    , DatePicker.view model.prev_date DatePicker.defaultSettings model.prev_date_state
                        |> Html.map PrevDateMsg
                    ]
                , Html.label []
                    [ Html.text "Next payment date:"
                    , DatePicker.view model.next_date DatePicker.defaultSettings model.next_date_state
                        |> Html.map NextDateMsg
                    ]
                , Html.label []
                    [ Html.text "Desired early payment date:"
                    , DatePicker.view model.early_date DatePicker.defaultSettings model.early_date_state
                        |> Html.map EarlyDateMsg
                    ]
                , Html.label []
                    [ Html.text "Desired total payment in this month:"
                    , plainInput DesiredSumChange
                    ]
                , Html.text
                    (case model.result of
                        Just result ->
                            "You should pay: " ++ String.fromFloat result

                        Nothing ->
                            ""
                    )
                , Html.pre []
                    [ Html.text
                        (case model.intermediate_results of
                            Just results ->
                                "R = "
                                    ++ String.fromFloat results.r
                                    ++ "\n"
                                    ++ "B = "
                                    ++ String.fromFloat results.b
                                    ++ "\n"
                                    ++ "X = "
                                    ++ String.fromFloat results.x
                                    ++ "\n"
                                    ++ "d1 = "
                                    ++ String.fromFloat results.d1
                                    ++ "\n"
                                    ++ "d2 = "
                                    ++ String.fromFloat results.d2
                                    ++ "\n"
                                    ++ "K1 = "
                                    ++ String.fromFloat results.k1
                                    ++ "\n"
                                    ++ "K2 = "
                                    ++ String.fromFloat results.k2
                                    ++ "\n"
                                    ++ "T = "
                                    ++ String.fromFloat results.t
                                    ++ "\n"

                            Nothing ->
                                ""
                        )
                    ]
                ]
            ]
        ]
    }
