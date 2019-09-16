module DateRecordParserTests exposing (dateTestSuite)

import DateFormat.Language as DateFormat
import DateRangePicker.DateRecordParser exposing (DateTimeParts, Input(..), InputDate(..), ParsingConfig, parseDateTime)
import Expect exposing (equal)
import Test exposing (Test, describe, test)
import Time.Extra


dateTestSuite : Test
dateTestSuite =
    describe "Parser Tests"
        [ test "can parse abbreviated month" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "Jan 01 2000")
        , test "can parse full month" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "January 01 2000")
        , test "can parse numerical month" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "1 01 2000")
        , test "can parse numerical padded month" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01 01 2000")
        , test "cannot parse invalid numerical month" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "13 01 2000")
        , test "cannot parse invalid numerical day" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "12 32 2000")
        , test "handle's leap year days" <|
            \_ ->
                equal
                    (date 2000 2 29)
                    (defaultParse "2 29 2000")
        , test "handle's invalid leap year days" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "2 29 2001")
        , test "can allow slash separator" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01/01/2000")
        , test "can allow dash separator" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01-01-2000")
        , test "can allow comma separator" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01 01, 2000")
        , test "does not allow double comma separator" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "01, 01, 2000")
        , test "can parse days with padded zero" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01 01, 2000")
        , test "can parse days without zero" <|
            \_ ->
                equal
                    (date 2000 1 1)
                    (defaultParse "01 1, 2000")
        , test "can parse days with double digits without a leading zero" <|
            \_ ->
                equal
                    (date 2000 1 11)
                    (defaultParse "01 11, 2000")
        , test "can parse years as two digits after 2000" <|
            \_ ->
                equal
                    (date 2020 1 1)
                    (defaultParse "01 01, 20")
        , test "can parse years as two digits before 2000" <|
            \_ ->
                equal
                    (date 1990 1 1)
                    (defaultParse "01 01, 90")
        , test "enforces minimum year requirement" <|
            \_ ->
                equal
                    (Err "Dates must occur after 1900")
                    (defaultParse "01 01, 1800")
        , test "enforces maximum year requirement" <|
            \_ ->
                equal
                    (Err "Dates must occur before 2100")
                    (defaultParse "01 01, 2500")
        , test "allows year to be by itself" <|
            \_ ->
                equal
                    (justYear 2000)
                    (defaultParse "2000")
        , test "does not allows year to be by itself when it is abbreviated" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "20")
        , test "allows year and month to be by themselves" <|
            \_ ->
                equal
                    (justYearAndMonth 2000 1)
                    (defaultParse "January 2000")
        , test "allows a time to be specified in 12 hour format" <|
            \_ ->
                equal
                    (dateTime 2000 1 1 1 0)
                    (defaultParse "01 01 2000 1:00 AM")
        , test "allows a time to be specified in 12 hour padded format" <|
            \_ ->
                equal
                    (dateTime 2000 1 1 1 0)
                    (defaultParse "01 01 2000 01:00 AM")
        , test "converts 12 AM to 24hour format" <|
            \_ ->
                equal
                    (dateTime 2000 1 1 0 0)
                    (defaultParse "01 01 2000 12:00 AM")
        , test "converts 1 PM to 24hour format" <|
            \_ ->
                equal
                    (dateTime 2000 1 1 13 0)
                    (defaultParse "01 01 2000 1:00 PM")
        , test "allows a time to be specified in 24 hour format" <|
            \_ ->
                equal
                    (dateTime 2000 1 1 0 0)
                    (defaultParse "01 01 2000 00:00")
        , test "does not allow minutes to exceed 59" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "01 01 2000 00:60")
        , test "does not allow hours to exceed 23 in 24hr format" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "01 01 2000 24:00")
        , test "does not allow hours to exceed 12 in 12hr format" <|
            \_ ->
                equal
                    (Err "Not a valid US Date!")
                    (defaultParse "01 01 2000 13:00 PM")
        , test "allows a date to be specified as a range" <|
            \_ ->
                equal
                    (rangeDate 2000 1 1 2000 1 1)
                    (defaultParse "01 01 2000 to 01 01 2000")
        , test "allows a date to be specified as a range with dash as separator" <|
            \_ ->
                equal
                    (rangeDate 2000 1 1 2000 1 1)
                    (defaultParse "01 01 2000 - 01 01 2000")
        , test "allows years to be specified as a range" <|
            \_ ->
                equal
                    (rangeYear 2000 2001)
                    (defaultParse "2000 - 2001")
        , test "allows datetime to be specified as a range" <|
            \_ ->
                equal
                    (rangeDateTime
                        { year = 2000, month = 1, day = 1, hour = 0, minute = 0 }
                        { year = 2000, month = 1, day = 1, hour = 23, minute = 59 }
                    )
                    (defaultParse "01 01 2000 00:00 - 01 01 2000 23:59")
        , test "allows range to have different formats on each end" <|
            \_ ->
                equal
                    (Ok <|
                        RangeInput
                            (JustYear 2000)
                            (FullDateTime { year = 2001, month = 1, day = 1, hour = 23, minute = 59 })
                    )
                    (defaultParse "2000 - 01 01 2001 23:59")
        , test "does not allow the end date to be before the start date" <|
            \_ ->
                equal
                    (Err "The starting date must be before the end date!")
                    (defaultParse "2001-2000")
        , test "allows a custom date string to be included" <|
            \_ ->
                equal
                    (Ok <| CustomDate "Past Month")
                    (parseDateTime { defaultParseConfig | customDateInputs = [ "Past Month" ] } "Past Month")
        , test "allows a custom date string to be written without spaces" <|
            \_ ->
                equal
                    (Ok <| CustomDate "Past Month")
                    (parseDateTime { defaultParseConfig | customDateInputs = [ "Past Month" ] } "PastMonth")
        , test "allows a custom date string to be written as lowercase" <|
            \_ ->
                equal
                    (Ok <| CustomDate "Past Month")
                    (parseDateTime { defaultParseConfig | customDateInputs = [ "Past Month" ] } "pastmonth")
        , test "allows a wildcard to be used as before a date" <|
            \_ ->
                equal
                    (Ok <| BeforeInput <| FullDate <| { year = 2000, month = 1, day = 1 })
                    (defaultParse "* to 01-01-2000")
        , test "allows a wildcard to be used after a date" <|
            \_ ->
                equal
                    (Ok <| AfterInput <| FullDate <| { year = 2000, month = 1, day = 1 })
                    (defaultParse "01-01-2000 to *")
        , test "allows a phrase to be used after a date to indicate before" <|
            \_ ->
                equal
                    (Ok <| BeforeInput <| FullDate <| { year = 2000, month = 1, day = 1 })
                    (defaultParse "01-01-2000 and before")
        , test "allows a phrase to be used after a date to indicate after" <|
            \_ ->
                equal
                    (Ok <| AfterInput <| FullDate <| { year = 2000, month = 1, day = 1 })
                    (defaultParse "01-01-2000 and after")
        ]


defaultParse : String -> Result String Input
defaultParse =
    parseDateTime defaultParseConfig


defaultParseConfig : ParsingConfig
defaultParseConfig =
    { customDateInputs = []
    , language =
        { toMonthName = DateFormat.english.toMonthName
        , toMonthAbbreviation = DateFormat.english.toMonthAbbreviation
        , am = "am"
        , pm = "pm"
        , to = "to"
        , andBefore = "And Before"
        , andAfter = "And After"
        }
    , allowTime = True
    }


justYear : Int -> Result String Input
justYear year =
    Ok <| SingleInput <| JustYear year


justYearAndMonth : Int -> Int -> Result String Input
justYearAndMonth year month =
    Ok <| SingleInput <| JustYearAndMonth { year = year, month = month }


date : Int -> Int -> Int -> Result String Input
date year month day =
    Ok <|
        SingleInput <|
            FullDate <|
                { year = year, month = month, day = day }


dateTime : Int -> Int -> Int -> Int -> Int -> Result String Input
dateTime year month day hour minute =
    Ok <|
        SingleInput <|
            FullDateTime <|
                { year = year, month = month, day = day, hour = hour, minute = minute }


rangeDate : Int -> Int -> Int -> Int -> Int -> Int -> Result String Input
rangeDate year1 month1 day1 year2 month2 day2 =
    Ok <|
        RangeInput
            (FullDate { year = year1, month = month1, day = day1 })
            (FullDate { year = year2, month = month2, day = day2 })


rangeYear : Int -> Int -> Result String Input
rangeYear year1 year2 =
    Ok <|
        RangeInput
            (JustYear year1)
            (JustYear year2)


rangeDateTime : DateTimeParts -> DateTimeParts -> Result String Input
rangeDateTime datetime1 datetime2 =
    Ok <|
        RangeInput
            (FullDateTime datetime1)
            (FullDateTime datetime2)
