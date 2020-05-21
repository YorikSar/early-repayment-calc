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
    { prevDate : Maybe Date
    , prevDateStr : String
    , prevDateState : DatePicker.Model
    , nextDate : Maybe Date
    , nextDateStr : String
    , nextDateState : DatePicker.Model
    , earlyDate : Maybe Date
    , earlyDateStr : String
    , earlyDateState : DatePicker.Model
    , debt : Maybe Float
    , debtStr : String
    , desiredSum : Maybe Float
    , desiredSumStr : String
    , rate : Maybe Float
    , rateStr : String
    , result : Maybe Float
    , intermediate_results : Maybe IntermediateResults
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { prevDate = Nothing
      , prevDateStr = ""
      , prevDateState = DatePicker.init
      , nextDate = Nothing
      , nextDateStr = ""
      , nextDateState = DatePicker.init
      , earlyDate = Nothing
      , earlyDateStr = ""
      , earlyDateState = DatePicker.init
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
                        | prevDateState = DatePicker.setToday date model.prevDateState
                        , nextDateState = DatePicker.setToday date model.nextDateState
                        , earlyDateState = DatePicker.setToday date model.earlyDateState
                        , earlyDate = Just date
                        , earlyDateStr = Date.toIsoString date
                    }

                PrevDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.prevDate model.prevDateState model.prevDateStr
                    in
                    { model | prevDate = date, prevDateState = state, prevDateStr = text }

                NextDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.nextDate model.nextDateState model.nextDateStr
                    in
                    { model | nextDate = date, nextDateState = state, nextDateStr = text }

                EarlyDateMsg dateMsg ->
                    let
                        ( date, state, text ) =
                            handlePicker dateMsg model.earlyDate model.earlyDateState model.earlyDateStr
                    in
                    { model | earlyDate = date, earlyDateState = state, earlyDateStr = text }

                DebtChange debtStr ->
                    { model | debt = stringToFloat debtStr, debtStr = debtStr }

                DesiredSumChange desiredSumStr ->
                    { model | desiredSum = stringToFloat desiredSumStr, desiredSumStr = desiredSumStr }

                RateChange rateStr ->
                    { model | rate = stringToFloat rateStr, rateStr = rateStr }

        calcResult =
            maybeMap6 calculate model.prevDate model.nextDate model.earlyDate model.debt model.desiredSum model.rate

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
                [ plainInput "Loan body:" model.debtStr DebtChange
                , plainInput "Yearly rate:" model.rateStr RateChange
                , datepicker "Previous payment date:" model.prevDate model.prevDateState model.prevDateStr PrevDateMsg
                , datepicker "Next payment date:" model.nextDate model.nextDateState model.nextDateStr NextDateMsg
                , datepicker "Desired early payment date:" model.earlyDate model.earlyDateState model.earlyDateStr EarlyDateMsg
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
