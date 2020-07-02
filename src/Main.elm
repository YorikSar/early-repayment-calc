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
    { prev : DatePicker
    , next : DatePicker
    , early : DatePicker
    , debt : Maybe Float
    , debtStr : String
    , desiredSum : Maybe Float
    , desiredSumStr : String
    , rate : Maybe Float
    , rateStr : String
    , result : Maybe Float
    , intermediate_results : Maybe IntermediateResults
    }


type alias DatePicker =
    { date : Maybe Date
    , text : String
    , state : DatePicker.Model
    }


initDatePicker : DatePicker
initDatePicker =
    { date = Nothing
    , text = ""
    , state = DatePicker.init
    }


type Msg
    = PrevDateMsg DatePicker.ChangeEvent
    | NextDateMsg DatePicker.ChangeEvent
    | EarlyDateMsg DatePicker.ChangeEvent
    | DebtChange String
    | DesiredSumChange String
    | RateChange String
    | SetToday Date


init : () -> ( Model, Cmd Msg )
init _ =
    ( { prev = initDatePicker
      , next = initDatePicker
      , early = initDatePicker
      , debt = Nothing
      , debtStr = ""
      , desiredSum = Nothing
      , desiredSumStr = ""
      , rate = Nothing
      , rateStr = ""
      , result = Nothing
      , intermediate_results = Nothing
      }
    , Task.perform SetToday Date.today
    )


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
calculate prevDate nextDate earlyDate debt desiredSum rate =
    let
        r =
            rate / 36500

        b =
            debt

        x =
            desiredSum

        d1 =
            toFloat (Date.diff Date.Days prevDate earlyDate)

        d2 =
            toFloat (Date.diff Date.Days earlyDate nextDate)

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


updateDatePicker : DatePicker.ChangeEvent -> DatePicker -> DatePicker
updateDatePicker msg dp =
    case msg of
        DatePicker.DateChanged newDate ->
            { dp
                | date = Just newDate
                , text = Date.toIsoString newDate
            }

        DatePicker.TextChanged newText ->
            { dp
                | date =
                    case Date.fromIsoString newText of
                        Ok newDate ->
                            Just newDate

                        Err _ ->
                            dp.date
                , text = newText
            }

        DatePicker.PickerChanged subMsg ->
            { dp
                | state = dp.state |> DatePicker.update subMsg
            }


setToday : Date -> DatePicker -> DatePicker
setToday date dp =
    { dp | state = DatePicker.setToday date dp.state }


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
                        | prev = setToday date model.prev
                        , next = setToday date model.next
                        , early = setToday date model.early |> updateDatePicker (DatePicker.DateChanged date)
                    }

                PrevDateMsg dateMsg ->
                    { model | prev = updateDatePicker dateMsg model.prev }

                NextDateMsg dateMsg ->
                    { model | next = updateDatePicker dateMsg model.next }

                EarlyDateMsg dateMsg ->
                    { model | early = updateDatePicker dateMsg model.early }

                DebtChange debtStr ->
                    { model | debt = stringToFloat debtStr, debtStr = debtStr }

                DesiredSumChange desiredSumStr ->
                    { model | desiredSum = stringToFloat desiredSumStr, desiredSumStr = desiredSumStr }

                RateChange rateStr ->
                    { model | rate = stringToFloat rateStr, rateStr = rateStr }

        calcResult =
            maybeMap6 calculate newModel.prev.date newModel.next.date newModel.early.date newModel.debt newModel.desiredSum newModel.rate

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


datepicker : String -> DatePicker -> (DatePicker.ChangeEvent -> Msg) -> Element.Element Msg
datepicker label dp msg =
    DatePicker.input []
        { onChange = msg
        , selected = dp.date
        , text = dp.text
        , label = Element.Input.labelAbove [] <| Element.text label
        , placeholder = Nothing
        , model = dp.state
        , settings = DatePicker.defaultSettings
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Early Repayment Calc"
    , body =
        [ Element.layout [] <|
            Element.column
                [ Element.centerX, Element.centerY, Element.spacing 10 ]
                [ plainInput "Loan body:" model.debtStr DebtChange
                , plainInput "Yearly rate:" model.rateStr RateChange
                , datepicker "Previous payment date:" model.prev PrevDateMsg
                , datepicker "Next payment date:" model.next NextDateMsg
                , datepicker "Desired early payment date:" model.early EarlyDateMsg
                , plainInput "Desired total payment in this month:" model.desiredSumStr DesiredSumChange
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
