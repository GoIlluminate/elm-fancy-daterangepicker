module DateRangePicker.Date exposing
    ( initDate, dateTuple, formatDate, formatDay, formatMonth, dayToInt, monthToInt, daysInMonth, startOfMonth, endOfMonth, startOfQuarter, endOfQuarter, monthAbbr
    , dateEqualTo, dateGreaterThan, dateLessThan, dateGreaterThanOrEqualTo, dateLessThanOrEqualTo
    )

{-| A custom Date Helper Library.

@docs initDate, dateTuple, formatDate, formatDay, formatMonth, dayToInt, monthToInt, daysInMonth, startOfMonth, endOfMonth, startOfQuarter, endOfQuarter, monthAbbr
@docs dateEqualTo, dateGreaterThan, dateLessThan, dateGreaterThanOrEqualTo, dateLessThanOrEqualTo

-}

import Date exposing (Date, format, fromCalendarDate)
import Time exposing (Month(..), Weekday(..))


{-| An opaque type to represent year as an Int.
-}
type alias Year =
    Int


{-| An opaque type to represent quarter as a triple of Months.
-}
type alias Quarter =
    ( Month, Month, Month )


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


{-| A function that takes a Date and returns the date representing the first date of the quarter that the passed in date belongs to.
-}
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
            1

        Mon ->
            2

        Tue ->
            3

        Wed ->
            4

        Thu ->
            5

        Fri ->
            6

        Sat ->
            7


{-| A function that takes a Month and returns it as an Int.

  - Ex. Jan -> 1
  - Ex. Dec -> 12

-}
monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


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
isLeapYear : Year -> Bool
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
