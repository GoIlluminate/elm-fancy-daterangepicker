module DateTests
    exposing
        ( dateTestSuite
        )

import Test
    exposing
        ( describe
        , test
        , Test
        )
import Expect
    exposing
        ( equal
        , Expectation
        )
import Date exposing (Date, Month(..))
import DateRangePicker.Date exposing (mkDate, daysInMonth, datesInRange, addDays, subDays, addMonths, subMonths, addYears, subYears, ($==), ($>=), ($<=), ($>), ($<))


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
        , describe "datesInRange Test"
            [ test "datesInRange returns all expected dates." <|
                \_ ->
                    testDatesInRange
                        (mkDate 2018 Jan 1)
                        (mkDate 2018 Jan 4)
                        [ mkDate 2018 Jan 1
                        , mkDate 2018 Jan 2
                        , mkDate 2018 Jan 3
                        , mkDate 2018 Jan 4
                        ]
            ]
        , describe "addDays/subDays Tests"
            [ test "Go to next year: addDays 1 (2017 Dec 31) -> (2018 Jan 1)" <|
                \_ ->
                    testAddDays
                        1
                        (mkDate 2017 Dec 31)
                        (mkDate 2018 Jan 1)
            , test "Go to next month (non leap year): addDays 1 (2018 Feb 28) -> (2018 Mar 1)" <|
                \_ ->
                    testAddDays
                        1
                        (mkDate 2018 Feb 28)
                        (mkDate 2018 Mar 1)
            , test "Stay in Feb (leap year): addDays 1 (2020 Feb 28) -> (2020 Feb 29)" <|
                \_ ->
                    testAddDays
                        1
                        (mkDate 2020 Feb 28)
                        (mkDate 2020 Feb 29)
            , test "Add more than 1 day: addDays 33 (2017 Dec 31) -> (2018 Feb 2)" <|
                \_ ->
                    testAddDays
                        33
                        (mkDate 2017 Dec 31)
                        (mkDate 2018 Feb 2)
            , test "Go to prev year: subDays 1 (2018 Jan 1) -> (2017 Dec 31)" <|
                \_ ->
                    testSubDays
                        1
                        (mkDate 2018 Jan 1)
                        (mkDate 2017 Dec 31)
            , test "Go to prev month (non leap year): subDays 1 (2018 Mar 1) -> (2018 Feb 28)" <|
                \_ ->
                    testSubDays
                        1
                        (mkDate 2018 Mar 1)
                        (mkDate 2018 Feb 28)
            , test "Go to prev month (leap year): subDays 1 (2020 Mar 1) -> (2020 Feb 29)" <|
                \_ ->
                    testSubDays
                        1
                        (mkDate 2020 Mar 1)
                        (mkDate 2020 Feb 29)
            , test "Sub more than 1 day: subDays 33 (2018 Feb 2) -> (2017 Dec 31)" <|
                \_ ->
                    testSubDays
                        33
                        (mkDate 2018 Feb 2)
                        (mkDate 2017 Dec 31)
            , test "addDays and subDays together: today -> addDays 10 <| subDays 10 today" <|
                \_ ->
                    equal today <| addDays 10 <| subDays 10 today
            ]
        , describe "addMonths/subMonths Tests"
            [ test "addMonths 1 (2018 Jan 1) -> (2018 Feb 1)" <|
                \_ ->
                    testAddMonths
                        1
                        (mkDate 2018 Jan 1)
                        (mkDate 2018 Feb 1)
            , test "addMonths 12 (2018 Jan 1) -> (2019 Jan 1)" <|
                \_ ->
                    testAddMonths
                        24
                        (mkDate 2018 Jan 1)
                        (mkDate 2020 Jan 1)
            , test "addMonths 1 (2020 Jan 29) -> (2020 Feb 29)" <|
                \_ ->
                    testAddMonths
                        1
                        (mkDate 2020 Jan 29)
                        (mkDate 2020 Feb 29)
            , test "addMonths 1 (2019 Jan 31) -> (2019 Feb 28)" <|
                \_ ->
                    testAddMonths
                        1
                        (mkDate 2019 Jan 31)
                        (mkDate 2019 Feb 28)
            , test "addMonths 1 (2019 Mar 31) -> (2019 Apr 30)" <|
                \_ ->
                    testAddMonths
                        1
                        (mkDate 2019 Mar 31)
                        (mkDate 2019 Apr 30)
            , test "subMonths 1 (2018 Feb 1) -> (2018 Jan 1)" <|
                \_ ->
                    testSubMonths
                        1
                        (mkDate 2018 Feb 1)
                        (mkDate 2018 Jan 1)
            , test "subMonths 12 (2019 Jan 1) -> (2018 Jan 1)" <|
                \_ ->
                    testSubMonths
                        24
                        (mkDate 2020 Jan 1)
                        (mkDate 2018 Jan 1)
            , test "subMonths 1 (2020 Feb 29) -> (2020 Jan 29)" <|
                \_ ->
                    testSubMonths
                        1
                        (mkDate 2020 Feb 29)
                        (mkDate 2020 Jan 29)
            , test "subMonths 1 (2019 Feb 28) -> (2019 Jan 28)" <|
                \_ ->
                    testSubMonths
                        1
                        (mkDate 2019 Feb 28)
                        (mkDate 2019 Jan 28)
            , test "subMonths 1 (2019 Apr 30) -> (2019 Mar 30)" <|
                \_ ->
                    testSubMonths
                        1
                        (mkDate 2019 Apr 30)
                        (mkDate 2019 Mar 30)
            , test "addMonths and subMonths together: today -> addMonths 10 <| subMonths 10 today" <|
                \_ ->
                    equal today <| addMonths 10 <| subMonths 10 today
            ]
        , describe "addYears/subYears Tests"
            [ test "addYears 1 (2018 Jan 1) -> (2019 Jan 1)" <|
                \_ ->
                    testAddYears
                        1
                        (mkDate 2018 Jan 1)
                        (mkDate 2019 Jan 1)
            , test "addYears 1 (2020 Feb 29) -> (2021 Feb 28)" <|
                \_ ->
                    testAddYears
                        1
                        (mkDate 2020 Feb 29)
                        (mkDate 2021 Feb 28)
            , test "addYears 10 (2018 Jan 1) -> (2028 Jan 1)" <|
                \_ ->
                    testAddYears
                        10
                        (mkDate 2018 Jan 1)
                        (mkDate 2028 Jan 1)
            , test "subYears 1 (2019 Jan 1) -> (2018 Jan 1)" <|
                \_ ->
                    testSubYears
                        1
                        (mkDate 2019 Jan 1)
                        (mkDate 2018 Jan 1)
            , test "subYears 1 (2020 Feb 29) -> (2019 Feb 28)" <|
                \_ ->
                    testSubYears
                        1
                        (mkDate 2020 Feb 29)
                        (mkDate 2019 Feb 28)
            , test "subYears 10 (2018 Jan 1) -> (2008 Jan 1)" <|
                \_ ->
                    testSubYears
                        10
                        (mkDate 2018 Jan 1)
                        (mkDate 2008 Jan 1)
            , test "addYears and subYears together: today -> addYears 10 <| subYears 10 today" <|
                \_ ->
                    equal today <| addYears 10 <| subYears 10 today
            ]
        , describe "Date Comparison Tests"
            [ test "Equal: True" <|
                \_ ->
                    equal True <| (mkDate 2018 Jan 1) $== (mkDate 2018 Jan 1)
            , test "Equal: False" <|
                \_ ->
                    equal False <| (mkDate 2018 Jan 2) $== (mkDate 2018 Jan 1)
            , test "Less than or equal to, same date: True" <|
                \_ ->
                    equal True <| (mkDate 2018 Jan 1) $<= (mkDate 2018 Jan 1)
            , test "Less than or equal to: True" <|
                \_ ->
                    equal True <| (mkDate 2017 Dec 31) $<= (mkDate 2018 Jan 1)
            , test "Less than or equal to: False" <|
                \_ ->
                    equal False <| (mkDate 2018 Dec 31) $<= (mkDate 2018 Jan 1)
            , test "Less than, same date: False" <|
                \_ ->
                    equal False <| (mkDate 2018 Jan 1) $< (mkDate 2018 Jan 1)
            , test "Less than: True" <|
                \_ ->
                    equal True <| (mkDate 2017 Dec 31) $< (mkDate 2018 Jan 1)
            , test "Less than: False" <|
                \_ ->
                    equal False <| (mkDate 2018 Dec 31) $< (mkDate 2018 Jan 1)
            , test "Greater than or equal to, same date: True" <|
                \_ ->
                    equal True <| (mkDate 2018 Jan 1) $>= (mkDate 2018 Jan 1)
            , test "Greater than or equal to: True" <|
                \_ ->
                    equal True <| (mkDate 2019 Jan 1) $>= (mkDate 2018 Jan 1)
            , test "Greater than or equal: False" <|
                \_ ->
                    equal False <| (mkDate 2017 Jan 1) $>= (mkDate 2018 Jan 1)
            , test "Greater than, same date: False" <|
                \_ ->
                    equal False <| (mkDate 2018 Jan 1) $> (mkDate 2018 Jan 1)
            , test "Greater than: True" <|
                \_ ->
                    equal True <| (mkDate 2019 Jan 1) $> (mkDate 2018 Jan 1)
            , test "Greater than: False" <|
                \_ ->
                    equal False <| (mkDate 2017 Jan 1) $> (mkDate 2018 Jan 1)
            ]
        ]


today : Date
today =
    mkDate 2018 Feb 20


testDaysInMonth : Int -> Month -> Int -> Expectation
testDaysInMonth year month output =
    daysInMonth year month |> equal output


testDatesInRange : Date -> Date -> List Date -> Expectation
testDatesInRange min max output =
    datesInRange min max |> equal output


testAddDays : Int -> Date -> Date -> Expectation
testAddDays n date output =
    addDays n date |> equal output


testSubDays : Int -> Date -> Date -> Expectation
testSubDays n date output =
    subDays n date |> equal output


testAddMonths : Int -> Date -> Date -> Expectation
testAddMonths n date output =
    addMonths n date |> equal output


testSubMonths : Int -> Date -> Date -> Expectation
testSubMonths n date output =
    subMonths n date |> equal output


testAddYears : Int -> Date -> Date -> Expectation
testAddYears n date output =
    addYears n date |> equal output


testSubYears : Int -> Date -> Date -> Expectation
testSubYears n date output =
    subYears n date |> equal output
