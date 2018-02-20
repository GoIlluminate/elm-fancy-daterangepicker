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
        , describe "isDisabledDate Tests"
            [ test "enabledDateRange is Nothing -> No disabled dates" <|
                \_ ->
                    testIsDisabledDate
                        Nothing
                        (mkDate 2018 Jan 3)
                        False
            , test "enabledDateRange has start and end, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange)
                        (mkDate 2018 Jan 2)
                        False
            , test "enabledDateRange has start and end, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange)
                        (mkDate 2018 Jan 4)
                        True
            , test "enabledDateRange has Nothing and end, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange2)
                        (mkDate 2018 Jan 2)
                        False
            , test "enabledDateRange has Nothing and end, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange2)
                        (mkDate 2018 Jan 4)
                        True
            , test "enabledDateRange has start and Nothing, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange3)
                        (mkDate 2018 Jan 1)
                        False
            , test "enabledDateRange has start and Nothing, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange3)
                        (mkDate 2017 Dec 31)
                        True
            ]
        ]


tEnabledDateRange : EnabledDateRange
tEnabledDateRange =
    { start = Just <| mkDate 2018 Jan 1
    , end = Just <| mkDate 2018 Jan 3
    }


tEnabledDateRange2 : EnabledDateRange
tEnabledDateRange2 =
    { start = Nothing
    , end = Just <| mkDate 2018 Jan 3
    }


tEnabledDateRange3 : EnabledDateRange
tEnabledDateRange3 =
    { start = Just <| mkDate 2018 Jan 1
    , end = Nothing
    }


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


testIsDisabledDate :
    Maybe EnabledDateRange
    -> Date
    -> Bool
    -> Expectation
testIsDisabledDate enabledDateRange date output =
    CI.isDisabledDate
        enabledDateRange
        date
        |> equal output
