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


type alias AllValues =
    { prev : Date
    , next : Date
    , early : Date
    , debt : Float
    , desiredSum : Float
    , rate : Float
    }


type alias MaybeValues =
    { prev : Maybe Date
    , next : Maybe Date
    , early : Maybe Date
    , debt : Maybe Float
    , desiredSum : Maybe Float
    , rate : Maybe Float
    }


type Values
    = AllSet AllValues Float IntermediateResults
    | NotAllSet MaybeValues


type alias States =
    { prev : DatePicker
    , next : DatePicker
    , early : DatePicker
    , debtStr : String
    , desiredSumStr : String
    , rateStr : String
    }


type alias Model =
    { values : Values
    , states : States
    }


maybeValuesFromValues : Values -> MaybeValues
maybeValuesFromValues vals =
    case vals of
        AllSet v _ _ ->
            { prev = Just v.prev
            , next = Just v.next
            , early = Just v.early
            , debt = Just v.debt
            , desiredSum = Just v.desiredSum
            , rate = Just v.rate
            }

        NotAllSet v ->
            v


maybeValuesToAllValues : MaybeValues -> Maybe AllValues
maybeValuesToAllValues v =
    maybeMap6 AllValues v.prev v.next v.early v.debt v.desiredSum v.rate


type alias DatePicker =
    { text : String
    , state : DatePicker.Model
    }


initDatePicker : DatePicker
initDatePicker =
    { text = ""
    , state = DatePicker.init
    }


setToday : Date -> DatePicker -> DatePicker
setToday date dp =
    { dp | state = DatePicker.setToday date dp.state }


updateDatePickerDate : DatePicker.ChangeEvent -> Maybe Date -> Maybe Date
updateDatePickerDate msg date =
    case msg of
        DatePicker.DateChanged newDate ->
            Just newDate

        DatePicker.TextChanged newText ->
            case Date.fromIsoString newText of
                Ok newDate ->
                    Just newDate

                Err _ ->
                    date

        DatePicker.PickerChanged subMsg ->
            date


updateDatePicker : DatePicker.ChangeEvent -> DatePicker -> DatePicker
updateDatePicker msg dp =
    case msg of
        DatePicker.DateChanged newDate ->
            { dp | text = Date.toIsoString newDate }

        DatePicker.TextChanged newText ->
            { dp | text = newText }

        DatePicker.PickerChanged subMsg ->
            { dp | state = dp.state |> DatePicker.update subMsg }


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
    ( { values = NotAllSet (MaybeValues Nothing Nothing Nothing Nothing Nothing Nothing)
      , states =
            { prev = initDatePicker
            , next = initDatePicker
            , early = initDatePicker
            , debtStr = ""
            , desiredSumStr = ""
            , rateStr = ""
            }
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


calculate : AllValues -> { result : Float, intermediate_results : IntermediateResults }
calculate { prev, next, early, debt, desiredSum, rate } =
    let
        r =
            rate / 36500

        b =
            debt

        x =
            desiredSum

        d1 =
            toFloat (Date.diff Date.Days prev early)

        d2 =
            toFloat (Date.diff Date.Days early next)

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


stringToFloat : String -> Maybe Float
stringToFloat s =
    s
        |> String.replace " " ""
        |> String.replace "," "."
        |> String.toFloat


updateValues : Msg -> MaybeValues -> MaybeValues
updateValues msg values =
    case msg of
        SetToday date ->
            { values | early = Just date }

        PrevDateMsg dateMsg ->
            { values | prev = updateDatePickerDate dateMsg values.prev }

        NextDateMsg dateMsg ->
            { values | next = updateDatePickerDate dateMsg values.next }

        EarlyDateMsg dateMsg ->
            { values | next = updateDatePickerDate dateMsg values.next }

        DebtChange debtStr ->
            { values | debt = stringToFloat debtStr }

        DesiredSumChange desiredSumStr ->
            { values | desiredSum = stringToFloat desiredSumStr }

        RateChange rateStr ->
            { values | rate = stringToFloat rateStr }


updateStates : Msg -> States -> States
updateStates msg states =
    case msg of
        SetToday date ->
            { states
                | prev = setToday date states.prev
                , next = setToday date states.next
                , early = setToday date states.early |> updateDatePicker (DatePicker.DateChanged date)
            }

        PrevDateMsg dateMsg ->
            { states | prev = updateDatePicker dateMsg states.prev }

        NextDateMsg dateMsg ->
            { states | next = updateDatePicker dateMsg states.next }

        EarlyDateMsg dateMsg ->
            { states | early = updateDatePicker dateMsg states.early }

        DebtChange debtStr ->
            { states | debtStr = debtStr }

        DesiredSumChange desiredSumStr ->
            { states | desiredSumStr = desiredSumStr }

        RateChange rateStr ->
            { states | rateStr = rateStr }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        values =
            maybeValuesFromValues model.values
                |> updateValues msg

        states =
            updateStates msg model.states

        newModel =
            case maybeValuesToAllValues values of
                Just allValues ->
                    let
                        { result, intermediate_results } =
                            calculate allValues
                    in
                    { values = AllSet allValues result intermediate_results
                    , states = states
                    }

                Nothing ->
                    { values = NotAllSet values
                    , states = states
                    }
    in
    ( newModel, Cmd.none )


plainInput : String -> String -> (String -> Msg) -> Element.Element Msg
plainInput label value msg =
    Element.Input.text []
        { onChange = msg
        , text = value
        , placeholder = Nothing
        , label = Element.Input.labelAbove [] <| Element.text label
        }


datepicker : String -> Maybe Date -> DatePicker -> (DatePicker.ChangeEvent -> Msg) -> Element.Element Msg
datepicker label date dp msg =
    DatePicker.input []
        { onChange = msg
        , selected = date
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
                (List.concat
                    [ [ plainInput "Loan body:" model.states.debtStr DebtChange
                      , plainInput "Yearly rate:" model.states.rateStr RateChange
                      ]
                    , let
                        { prev, next, early } =
                            case model.values of
                                AllSet v _ _ ->
                                    { prev = Just v.prev, next = Just v.next, early = Just v.early }

                                NotAllSet v ->
                                    { prev = v.prev, next = v.next, early = v.early }
                      in
                      [ datepicker "Previous payment date:" prev model.states.prev PrevDateMsg
                      , datepicker "Next payment date:" next model.states.next NextDateMsg
                      , datepicker "Desired early payment date:" early model.states.early EarlyDateMsg
                      ]
                    , [ plainInput "Desired total payment in this month:" model.states.desiredSumStr DesiredSumChange ]
                    , case model.values of
                        NotAllSet _ ->
                            []

                        AllSet _ result ir ->
                            [ Element.text <| "You should pay: " ++ String.fromFloat result
                            , Element.text <|
                                "R = "
                                    ++ String.fromFloat ir.r
                                    ++ "\n"
                                    ++ "B = "
                                    ++ String.fromFloat ir.b
                                    ++ "\n"
                                    ++ "X = "
                                    ++ String.fromFloat ir.x
                                    ++ "\n"
                                    ++ "d1 = "
                                    ++ String.fromFloat ir.d1
                                    ++ "\n"
                                    ++ "d2 = "
                                    ++ String.fromFloat ir.d2
                                    ++ "\n"
                                    ++ "K1 = "
                                    ++ String.fromFloat ir.k1
                                    ++ "\n"
                                    ++ "K2 = "
                                    ++ String.fromFloat ir.k2
                                    ++ "\n"
                                    ++ "T = "
                                    ++ String.fromFloat ir.t
                                    ++ "\n"
                            ]
                    ]
                )
        ]
    }
