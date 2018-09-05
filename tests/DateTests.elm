module DateTests exposing (dateTestSuite)

import Date exposing (Date, fromCalendarDate)
import DateRangePicker.Date
    exposing
        ( dateEqualTo
        , dateGreaterThan
        , dateGreaterThanOrEqualTo
        , dateLessThan
        , dateLessThanOrEqualTo
        , daysInMonth
        )
import Expect
    exposing
        ( Expectation
        , equal
        )
import Test
    exposing
        ( Test
        , describe
        , test
        )
import Time exposing (Month(..), Weekday(..))


dateTestSuite : Test
dateTestSuite =
    describe "Common.Internal Tests"
        [ describe "daysInMonth Leap Year Tests"
            [ test "Feb 2019 has 28 days" <|
                \_ ->
                    testDaysInMonth 2019 Feb 28
            , test "Feb 2020 has 29 days" <|
                \_ ->
                    testDaysInMonth 2020 Feb 29
            ]
        , describe "Date Comparison Tests"
            [ test "Equal: True" <|
                \_ ->
                    equal True <| dateEqualTo (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Equal: False" <|
                \_ ->
                    equal False <| dateEqualTo (fromCalendarDate 2018 Jan 2) (fromCalendarDate 2018 Jan 1)
            , test "Less than or equal to, same date: True" <|
                \_ ->
                    equal True <| dateLessThanOrEqualTo (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Less than or equal to: True" <|
                \_ ->
                    equal True <| dateLessThanOrEqualTo (fromCalendarDate 2017 Dec 31) (fromCalendarDate 2018 Jan 1)
            , test "Less than or equal to: False" <|
                \_ ->
                    equal False <| dateLessThanOrEqualTo (fromCalendarDate 2018 Dec 31) (fromCalendarDate 2018 Jan 1)
            , test "Less than, same date: False" <|
                \_ ->
                    equal False <| dateLessThan (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Less than: True" <|
                \_ ->
                    equal True <| dateLessThan (fromCalendarDate 2017 Dec 31) (fromCalendarDate 2018 Jan 1)
            , test "Less than: False" <|
                \_ ->
                    equal False <| dateLessThan (fromCalendarDate 2018 Dec 31) (fromCalendarDate 2018 Jan 1)
            , test "Greater than or equal to, same date: True" <|
                \_ ->
                    equal True <| dateGreaterThanOrEqualTo (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Greater than or equal to: True" <|
                \_ ->
                    equal True <| dateGreaterThanOrEqualTo (fromCalendarDate 2019 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Greater than or equal: False" <|
                \_ ->
                    equal False <| dateGreaterThanOrEqualTo (fromCalendarDate 2017 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Greater than, same date: False" <|
                \_ ->
                    equal False <| dateGreaterThan (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Greater than: True" <|
                \_ ->
                    equal True <| dateGreaterThan (fromCalendarDate 2019 Jan 1) (fromCalendarDate 2018 Jan 1)
            , test "Greater than: False" <|
                \_ ->
                    equal False <| dateGreaterThan (fromCalendarDate 2017 Jan 1) (fromCalendarDate 2018 Jan 1)
            ]
        ]


today : Date
today =
    fromCalendarDate 2018 Feb 20


testDaysInMonth : Int -> Month -> Int -> Expectation
testDaysInMonth year month output =
    daysInMonth year month |> equal output
