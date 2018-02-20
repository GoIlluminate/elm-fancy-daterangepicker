module CommonInternalTests
    exposing
        ( commonInternalTestSuite
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
import DateRangePicker.Common as Common exposing (DateRange, mkDateRange)
import DateRangePicker.Common.Internal as CI exposing (EnabledDateRange)
import DateRangePicker.Date exposing (mkDate)


commonInternalTestSuite : Test
commonInternalTestSuite =
    describe "Common.Internal Tests"
        [ describe "inRange Tests"
            [ test "test date in range" <|
                \_ ->
                    testInRange
                        (mkDate 2018 Jan 2)
                        (mkDateRange (mkDate 2018 Jan 1) (mkDate 2018 Jan 3))
                        True
            , test "test start date in range" <|
                \_ ->
                    testInRange
                        (mkDate 2018 Jan 1)
                        (mkDateRange (mkDate 2018 Jan 1) (mkDate 2018 Jan 3))
                        True
            , test "test end date in range" <|
                \_ ->
                    testInRange
                        (mkDate 2018 Jan 3)
                        (mkDateRange (mkDate 2018 Jan 1) (mkDate 2018 Jan 3))
                        True
            , test "test date not in range 1" <|
                \_ ->
                    testInRange
                        (mkDate 2018 Jan 4)
                        (mkDateRange (mkDate 2018 Jan 1) (mkDate 2018 Jan 3))
                        False
            , test "test date not in range 2" <|
                \_ ->
                    testInRange
                        (mkDate 2017 Jan 1)
                        (mkDateRange (mkDate 2018 Jan 1) (mkDate 2018 Jan 3))
                        False
            ]
        ]


testInRange :
    Date
    -> DateRange
    -> Bool
    -> Expectation
testInRange date dateRange output =
    CI.inRange
        date
        dateRange
        |> equal output
