module Tests exposing (..)

import Main
import Date
import Date.Extra
import Expect
import Test exposing (Test, test, describe)


getDate y m d =
    Date.Extra.fromParts y m d 0 0 0 0


suite : Test
suite =
    describe "calculate"
        [ test "first" <|
            \_ ->
                let
                    { result, intermediate_results } =
                        Main.calculate
                            (getDate 2017 Date.Jul 1)
                            (getDate 2017 Date.Jul 31)
                            (getDate 2017 Date.Jul 29)
                            1000000
                            40000
                            36.5
                in
                    Expect.equal (floor (result * 100)) 3802004
        ]
