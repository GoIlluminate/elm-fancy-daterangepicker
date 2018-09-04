module CommonInternalTests exposing (commonInternalTestSuite)

import Date exposing (Date, fromCalendarDate)
import DateRangePicker.Common as Common exposing (DateRange, RestrictedDateRange(..), inRange, mkDateRange)
import DateRangePicker.Common.Internal as CI exposing (EnabledDateRange, mkEnabledDateRangeFromRestrictedDateRange)
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


commonInternalTestSuite : Test
commonInternalTestSuite =
    describe "Common.Internal Tests"
        [ describe "inRange Tests"
            [ test "test date in range" <|
                \_ ->
                    testInRange
                        (fromCalendarDate 2018 Jan 2)
                        (mkDateRange (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 3))
                        True
            , test "test start date in range" <|
                \_ ->
                    testInRange
                        (fromCalendarDate 2018 Jan 1)
                        (mkDateRange (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 3))
                        True
            , test "test end date in range" <|
                \_ ->
                    testInRange
                        (fromCalendarDate 2018 Jan 3)
                        (mkDateRange (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 3))
                        True
            , test "test date not in range 1" <|
                \_ ->
                    testInRange
                        (fromCalendarDate 2018 Jan 4)
                        (mkDateRange (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 3))
                        False
            , test "test date not in range 2" <|
                \_ ->
                    testInRange
                        (fromCalendarDate 2017 Jan 1)
                        (mkDateRange (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Jan 3))
                        False
            ]
        , describe "isDisabledDate Tests"
            [ test "enabledDateRange is Nothing -> No disabled dates" <|
                \_ ->
                    testIsDisabledDate
                        Nothing
                        (fromCalendarDate 2018 Jan 3)
                        False
            , test "enabledDateRange has start and end, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange)
                        (fromCalendarDate 2018 Jan 2)
                        False
            , test "enabledDateRange has start and end, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange)
                        (fromCalendarDate 2018 Jan 4)
                        True
            , test "enabledDateRange has Nothing and end, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange2)
                        (fromCalendarDate 2018 Jan 2)
                        False
            , test "enabledDateRange has Nothing and end, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange2)
                        (fromCalendarDate 2018 Jan 4)
                        True
            , test "enabledDateRange has start and Nothing, date is not disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange3)
                        (fromCalendarDate 2018 Jan 1)
                        False
            , test "enabledDateRange has start and Nothing, date is disabled" <|
                \_ ->
                    testIsDisabledDate
                        (Just tEnabledDateRange3)
                        (fromCalendarDate 2017 Dec 31)
                        True
            ]
        , describe "isDisabledDate From RestrictedDateRange Tests"
            [ test "Off -> No disabled dates before" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Off
                        (fromCalendarDate 2018 Feb 19)
                        False
            , test "Off -> No disabled dates after" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Off
                        (fromCalendarDate 2018 Feb 21)
                        False
            , test "ToPresent -> yesterday is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        ToPresent
                        yesterday
                        False
            , test "ToPresent -> today is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        ToPresent
                        today
                        False
            , test "ToPresent -> tomorrow is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        ToPresent
                        tomorrow
                        True
            , test "FromPresent -> yesterday is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        FromPresent
                        yesterday
                        True
            , test "FromPresent -> today is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        FromPresent
                        today
                        False
            , test "FromPresent -> tomorrow is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        FromPresent
                        tomorrow
                        False
            , test "Past -> yesterday is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Past
                        yesterday
                        False
            , test "Past -> today is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Past
                        today
                        True
            , test "Past -> tomorrow is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Past
                        tomorrow
                        True
            , test "Future -> yesterday is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Future
                        yesterday
                        True
            , test "Future -> today is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Future
                        today
                        True
            , test "Future -> tomorrow is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        Future
                        tomorrow
                        False
            , test "Between yesterday tomorrow -> today is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (Between yesterday tomorrow)
                        today
                        False
            , test "Between today tomorrow -> yesterday is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (Between today tomorrow)
                        yesterday
                        True
            , test "Between yesterday today -> tomorrow is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (Between yesterday today)
                        tomorrow
                        True
            , test "To (2018 Jan 1) -> (2018 Jan 1) is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (To (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2018 Jan 1)
                        False
            , test "To (2018 Jan 1) -> (2017 Dec 31) is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (To (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2017 Dec 31)
                        False
            , test "To (2018 Jan 1) -> (2018 Jan 2) is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (To (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2018 Jan 2)
                        True
            , test "From (2018 Jan 1) -> (2018 Jan 1) is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (From (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2018 Jan 1)
                        False
            , test "From (2018 Jan 1) -> (2017 Dec 31) is disabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (From (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2017 Dec 31)
                        True
            , test "From (2018 Jan 1) -> (2018 Jan 2) is enabled" <|
                \_ ->
                    testIsDisabledDateFromRestrictedDateRange
                        (From (fromCalendarDate 2018 Jan 1))
                        (fromCalendarDate 2018 Jan 2)
                        False
            ]
        ]


today : Date
today =
    fromCalendarDate 2018 Feb 20


yesterday : Date
yesterday =
    Date.add Date.Days -1 today


tomorrow : Date
tomorrow =
    Date.add Date.Days 1 today


tEnabledDateRange : EnabledDateRange
tEnabledDateRange =
    { start = Just <| fromCalendarDate 2018 Jan 1
    , end = Just <| fromCalendarDate 2018 Jan 3
    }


tEnabledDateRange2 : EnabledDateRange
tEnabledDateRange2 =
    { start = Nothing
    , end = Just <| fromCalendarDate 2018 Jan 3
    }


tEnabledDateRange3 : EnabledDateRange
tEnabledDateRange3 =
    { start = Just <| fromCalendarDate 2018 Jan 1
    , end = Nothing
    }


testInRange :
    Date
    -> DateRange
    -> Bool
    -> Expectation
testInRange date dateRange output =
    inRange
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


testIsDisabledDateFromRestrictedDateRange :
    RestrictedDateRange
    -> Date
    -> Bool
    -> Expectation
testIsDisabledDateFromRestrictedDateRange restrictedDateRange date output =
    let
        enabledDateRange =
            mkEnabledDateRangeFromRestrictedDateRange restrictedDateRange today
    in
    CI.isDisabledDate
        enabledDateRange
        date
        |> equal output
