module Main exposing (calculate, main)

import Browser
import Date exposing (Date)
import DatePicker
import Element
import Element.Input
import Maybe
import Task


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { prev_date : Maybe Date
    , prev_date_str : String
    , prev_date_state : DatePicker.Model
    , next_date : Maybe Date
    , next_date_str : String
    , next_date_state : DatePicker.Model
    , early_date : Maybe Date
    , early_date_str : String
    , early_date_state : DatePicker.Model
    , debt : Maybe Float
    , debt_str : String
    , desired_sum : Maybe Float
    , desired_sum_str : String
    , rate : Maybe Float
    , rate_str : String
    , result : Maybe Float
    , intermediate_results : Maybe IntermediateResults
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { prev_date = Nothing
      , prev_date_str = ""
      , prev_date_state = DatePicker.init
      , next_date = Nothing
      , next_date_str = ""
      , next_date_state = DatePicker.init
      , early_date = Nothing
      , early_date_str = ""
      , early_date_state = DatePicker.init
      , debt = Nothing
      , debt_str = ""
      , desired_sum = Nothing
      , desired_sum_str = ""
      , rate = Nothing
      , rate_str = ""
      , result = Nothing
      , intermediate_results = Nothing
      }
    , Task.perform SetToday Date.today
    )


type Msg
    = PrevDateMsg DatePicker.ChangeEvent
    | NextDateMsg DatePicker.ChangeEvent
    | EarlyDateMsg DatePicker.ChangeEvent
    | DebtChange String
    | DesiredSumChange String
    | RateChange String
    | SetToday Date


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    Maybe.andThen (\a -> Maybe.map5 (func a) mb mc md me mf) ma


type alias IntermediateResults =
    { r : Float
    , b : Float
    , x : Float
    , d1 : Float
    , d2 : Float
    , k1 : Float
    , k2 : Float
    , t : Float
    }


calculate : Date -> Date -> Date -> Float -> Float -> Float -> { result : Float, intermediate_results : IntermediateResults }
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


handlePicker : DatePicker.ChangeEvent -> Maybe Date -> DatePicker.Model -> String -> ( Maybe Date, DatePicker.Model, String )
handlePicker msg date state text =
    case msg of
        DatePicker.DateChanged newDate ->
            ( Just newDate
            , state
            , Date.toIsoString newDate
            )

        DatePicker.TextChanged newText ->
            ( case Date.fromIsoString newText of
                Ok newDate ->
                    Just newDate

                Err _ ->
                    date
            , state
            , newText
            )

        DatePicker.PickerChanged subMsg ->
            ( date
            , state |> DatePicker.update subMsg
            , text
            )


stringToFloat : String -> Maybe Float
stringToFloat s =
    s
        |> String.replace " " ""
        |> String.replace "," "."
        |> String.toFloat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                SetToday date ->
                    { model
                        | prev_date_state = DatePicker.setToday date model.prev_date_state
                        , next_date_state = DatePicker.setToday date model.next_date_state
                        , early_date_state = DatePicker.setToday date model.early_date_state
                        , early_date = Just date
                        , early_date_str = Date.toIsoString date
                    }

                PrevDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.prev_date model.prev_date_state model.prev_date_str
                    in
                    { model | prev_date = date, prev_date_state = state, prev_date_str = text }

                NextDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.next_date model.next_date_state model.next_date_str
                    in
                    { model | next_date = date, next_date_state = state, next_date_str = text }

                EarlyDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.early_date model.early_date_state model.early_date_str
                    in
                    { model | early_date = date, early_date_state = state, early_date_str = text }

                DebtChange debtStr ->
                    { model | debt = stringToFloat debtStr, debt_str = debtStr }

                DesiredSumChange desiredSumStr ->
                    { model | desired_sum = stringToFloat desiredSumStr, desired_sum_str = desiredSumStr }

                RateChange rateStr ->
                    { model | rate = stringToFloat rateStr, rate_str = rateStr }

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


plainInput : String -> String -> (String -> Msg) -> Element.Element Msg
plainInput label value msg =
    Element.Input.text []
        { onChange = msg
        , text = value
        , placeholder = Nothing
        , label = Element.Input.labelAbove [] <| Element.text label
        }


datepicker : String -> Maybe Date -> DatePicker.Model -> String -> (DatePicker.ChangeEvent -> Msg) -> Element.Element Msg
datepicker label date state text msg =
    DatePicker.input []
        { onChange = msg
        , selected = date
        , text = text
        , label = Element.Input.labelAbove [] <| Element.text label
        , placeholder = Nothing
        , model = state
        , settings = DatePicker.defaultSettings
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Early Repayment Calc"
    , body =
        [ Element.layout [] <|
            Element.column
                [ Element.centerX, Element.centerY, Element.spacing 10 ]
                [ plainInput "Loan body:" model.debt_str DebtChange
                , plainInput "Yearly rate:" model.rate_str RateChange
                , datepicker "Previous payment date:" model.prev_date model.prev_date_state model.prev_date_str PrevDateMsg
                , datepicker "Next payment date:" model.next_date model.next_date_state model.next_date_str NextDateMsg
                , datepicker "Desired early payment date:" model.early_date model.early_date_state model.early_date_str EarlyDateMsg
                , plainInput "Desired total payment in this month:" model.desired_sum_str DesiredSumChange
                , Element.text
                    (case model.result of
                        Just result ->
                            "You should pay: " ++ String.fromFloat result

                        Nothing ->
                            ""
                    )
                , Element.text
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
    }
