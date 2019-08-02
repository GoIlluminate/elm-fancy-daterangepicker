module DateRangePicker.Helper exposing (chunksOfLeft, dateEqualTo, dateGreaterThan, dateGreaterThanOrEqualTo, dateLessThan, dateLessThanOrEqualTo, dateTuple, dayToInt, daysInMonth, daysInRange, endOfMonth, endOfQuarter, formatDate, formatDay, formatMonth, getQuarter, inRange, initDate, isDisabledDate, isLeapYear, mkDateRange, monthAbbr, monthToInt, monthsInRange, mouseDownNoDefault, mouseUpNoDefault, noPresets, onClickNoDefault, padMonthLeft, padMonthRight, renderDaysOfWeek, startOfMonth, startOfQuarter, weeksInRange, yearsInRange)

import Date exposing (Date, Unit(..), format, fromCalendarDate)
import DateRangePicker.Types exposing (CalendarRange, DateRange, EnabledDateRange, Quarter, Year)
import Html exposing (Html, div, span, td, text)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import List.Extra as LE
import Time exposing (Month(..), Weekday(..))


{-| A function that initializes a date.
-}
initDate : Date
initDate =
    fromCalendarDate 1994 Dec 6


{-| A function to format a Date into a string.

  - Ex. Feb 15, 2018

-}
formatDate : Date -> String
formatDate date =
    format "MMMM ddd, y" date


{-| A function that takes a Weekday and formats it into the abbreviated string.

  - Ex. Mon -> "Mo"

-}
formatDay : Weekday -> String
formatDay day =
    case day of
        Mon ->
            "Mo"

        Tue ->
            "Tu"

        Wed ->
            "We"

        Thu ->
            "Th"

        Fri ->
            "Fr"

        Sat ->
            "Sa"

        Sun ->
            "Su"


{-| A function that formats a Month into the full string.

  - Ex. Jan -> "January"

-}
formatMonth : Month -> String
formatMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


{-| A function that formats a Month into the abbreviated string.

  - Ex. Jan -> "Jan"

-}
monthAbbr : Month -> String
monthAbbr month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


{-| A function that converts a Date into a tuple. (year, month, day)
-}
dateTuple : Date -> ( Int, Int, Int )
dateTuple date =
    ( Date.year date, monthToInt <| Date.month date, Date.day date )


{-| A function that takes a Date and returns the date representing the first of that month.
-}
startOfMonth : Date -> Date
startOfMonth date =
    fromCalendarDate (Date.year date) (Date.month date) 1


{-| A function that takes a Date and returns the date representing the end of that month.
-}
endOfMonth : Date -> Date
endOfMonth date =
    let
        y =
            Date.year date

        m =
            Date.month date
    in
    fromCalendarDate y m (daysInMonth y m)


startOfQuarter : Date -> Date
startOfQuarter date =
    let
        ( startMonth, _, _ ) =
            getQuarter date
    in
    fromCalendarDate (Date.year date) startMonth 1


{-| A function that takes a Date and returns the date representing the last date of the quarter that the passed in date belongs to.
-}
endOfQuarter : Date -> Date
endOfQuarter date =
    let
        year =
            Date.year date

        ( _, _, endMonth ) =
            getQuarter date
    in
    fromCalendarDate year endMonth (daysInMonth year endMonth)


{-| A function that takes a Weekday and returns it as an integer.

  - Sun - Sat
  - 1 - 7

-}
dayToInt : Weekday -> Int
dayToInt day =
    case day of
        Sun ->
            0

        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6


{-| A function that takes a Month and returns it as an Int.

  - Ex. Jan -> 1
  - Ex. Dec -> 12

-}
monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


{-| A function that returns the number of days in the given month for the given year.
-}
daysInMonth : Year -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


{-| An opaque function to determine whether a given year is a leap year.
-}
isLeapYear : DateRangePicker.Types.Year -> Bool
isLeapYear year =
    modBy 400 year == 0 || modBy 100 year /= 0 && modBy 4 year == 0


{-| A function that checks if date a is equal to date b
-}
dateEqualTo : Date -> Date -> Bool
dateEqualTo a b =
    Date.toRataDie a == Date.toRataDie b


{-| A function that checks if date a is less than or equal to date b
-}
dateLessThanOrEqualTo : Date -> Date -> Bool
dateLessThanOrEqualTo a b =
    Date.toRataDie a <= Date.toRataDie b


{-| A function that checks if date a is greater than or equal to date b
-}
dateGreaterThanOrEqualTo : Date -> Date -> Bool
dateGreaterThanOrEqualTo a b =
    Date.toRataDie a >= Date.toRataDie b


{-| A function that checks if date a is less than to date b
-}
dateLessThan : Date -> Date -> Bool
dateLessThan a b =
    Date.toRataDie a < Date.toRataDie b


{-| A function that checks if date a is greater than to date b
-}
dateGreaterThan : Date -> Date -> Bool
dateGreaterThan a b =
    Date.toRataDie a > Date.toRataDie b


{-| A function that takes a date and returns the quarter that it belongs to.
-}
getQuarter : Date -> Quarter
getQuarter date =
    let
        q1 =
            ( Jan, Feb, Mar )

        q2 =
            ( Apr, May, Jun )

        q3 =
            ( Jul, Aug, Sep )

        q4 =
            ( Oct, Nov, Dec )

        month =
            Date.month date
    in
    case month of
        Jan ->
            q1

        Feb ->
            q1

        Mar ->
            q1

        Apr ->
            q2

        May ->
            q2

        Jun ->
            q2

        Jul ->
            q3

        Aug ->
            q3

        Sep ->
            q3

        Oct ->
            q4

        Nov ->
            q4

        Dec ->
            q4



-----------------------------------------------------------------------------------------------------------------------


mkDateRange : Date -> Date -> DateRange
mkDateRange start end =
    { start = start, end = end }


inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    dateLessThanOrEqualTo start date && dateGreaterThanOrEqualTo end date


yearsInRange : DateRange -> Int
yearsInRange dateRange =
    Date.diff Date.Years dateRange.start dateRange.end


monthsInRange : DateRange -> Int
monthsInRange dateRange =
    Date.diff Date.Months dateRange.start dateRange.end


weeksInRange : DateRange -> Int
weeksInRange dateRange =
    Date.diff Date.Weeks dateRange.start dateRange.end


daysInRange : DateRange -> Int
daysInRange dateRange =
    Date.diff Date.Days dateRange.start dateRange.end



-----------------------------------------------------------------------------------------------------------------------


{-| An opaque function to check if the given date is a disabled date.
-}
isDisabledDate : Maybe EnabledDateRange -> Date -> Bool
isDisabledDate enabledDateRange date =
    case enabledDateRange of
        Nothing ->
            False

        Just dateRange ->
            case ( dateRange.start, dateRange.end ) of
                ( Just start, Just end ) ->
                    not <| inRange date <| { start = start, end = end }

                ( Just start, Nothing ) ->
                    dateLessThan date start

                ( Nothing, Just end ) ->
                    dateGreaterThan date end

                ( Nothing, Nothing ) ->
                    False


{-| An opaque function taht pads the month from the left with filler days
in order to get the first of the month to line up correctly with the correct
day of the week.
-}
padMonthLeft : Date -> List (Html msg)
padMonthLeft d =
    let
        lastPosition =
            if emptySpots >= 0 then
                [ getFiller True ]

            else
                []

        getFiller b =
            td
                [ Attrs.classList
                    [ ( "elm-fancy-daterangepicker--day-filler", True )
                    , ( "border-b", True )
                    , ( "border-r", b )
                    ]
                ]
                []

        emptySpots =
            (Date.weekday d |> dayToInt) - 1
    in
    List.repeat emptySpots (getFiller False) ++ lastPosition


{-| An opaque function that pads the end of the month with filler days in order to fill
the month with 42 total days (days + filler days) to keep the size of each month in the
calendar the same size.
-}
padMonthRight : Int -> List (Html msg)
padMonthRight n =
    td [ Attrs.class "elm-fancy-daterangepicker--day-filler" ] []
        |> List.repeat n


{-| An opaque function that gets the Days of the Week Html Msg for the calendar.
-}
renderDaysOfWeek : List (Html msg)
renderDaysOfWeek =
    let
        days =
            7 :: List.range 1 6

        dayOfWeek n =
            td [ Attrs.class "elm-fancy-daterangepicker--dow" ]
                [ text <|
                    formatDay <|
                        Date.numberToWeekday n
                ]
    in
    List.map dayOfWeek days


{-| An opaque recursive function that chunks a list of a into a
list of lists of a of equal chunks.


## Example:

    chunksOfLeft 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] =
        [ [ 1, 2, 3 ]
        , [ 4, 5, 6 ]
        , [ 7, 8, 9 ]
        , [ 10, 11, 12 ]
        ]

-}
chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
    let
        len =
            List.length xs
    in
    if len > k then
        List.take k xs :: chunksOfLeft k (List.drop k xs)

    else
        [ xs ]


{-| An opaque function that prevents default click events.
-}
onClickNoDefault : msg -> Html.Attribute msg
onClickNoDefault message =
    Events.custom "click" <|
        Json.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }


{-| An opaque function that prevents default click events.
-}
mouseDownNoDefault : msg -> Html.Attribute msg
mouseDownNoDefault message =
    Events.custom "mousedown" <|
        Json.succeed
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }


{-| An opaque function that prevents default click events.
-}
mouseUpNoDefault : msg -> Html.Attribute msg
mouseUpNoDefault message =
    Events.custom "mouseup" <|
        Json.succeed
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }


{-| An opaque function that returns a no presets available Html msg
-}
noPresets : List (Html msg)
noPresets =
    [ text "" ]
