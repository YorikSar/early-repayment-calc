module Tests exposing (..)

import Date
import Expect
import Main
import Test exposing (Test, describe, test)
import Time


getDate y m d =
    Date.fromCalendarDate y m d


suite : Test
suite =
    describe "calculate"
        [ test "first" <|
            \_ ->
                let
                    { result, intermediate_results } =
                        Main.calculate
                            (getDate 2017 Time.Jul 1)
                            (getDate 2017 Time.Jul 31)
                            (getDate 2017 Time.Jul 29)
                            1000000
                            40000
                            36.5
                in
                Expect.equal (floor (result * 100)) 3802004
        ]
