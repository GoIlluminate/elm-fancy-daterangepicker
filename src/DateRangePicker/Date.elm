module DateRangePicker.Date
    exposing
        ( initDate
        , mkDate
        , dateTuple
        , formatDate
        , formatDay
        , formatMonth
        , dayToInt
        , dayFromInt
        , monthToInt
        , monthFromInt
        , daysInMonth
        , datesInRange
        , startOfMonth
        , endOfMonth
        , addDays
        , subDays
        , addMonths
        , subMonths
        , addYears
        , subYears
        , ($==)
        , ($<=)
        , ($>=)
        , ($<)
        , ($>)
        )

{-| A custom Date Helper Library.

@docs initDate, mkDate, dateTuple, formatDate, formatDay, formatMonth, dayToInt, dayFromInt, monthToInt, monthFromInt, daysInMonth, datesInRange, startOfMonth, endOfMonth, addDays, subDays, addMonths, subMonths, addYears, subYears
@docs ($<), ($<=), ($==), ($>), ($>=)

-}

import Date exposing (Date, Day(..), Month(..), year, month, day)


{-| An opaque type to represent year as an Int.
-}
type alias Year =
    Int


{-| An opaque type to represent day as an Int.
-}
type alias Day =
    Int


{-| A function that initializes a date.
-}
initDate : Date
initDate =
    mkDate 1992 May 29


{-| A function to format a Date into a string.

  - Ex. Feb 15, 2018

-}
formatDate : Date -> String
formatDate date =
    String.concat
        [ toString (month date)
        , " "
        , dayToString (day date)
        , ", "
        , toString (year date)
        ]


{-| A function that takes a Date.Day and formats it into the abbreviated string.

  - Ex. Mon -> "Mo"

-}
formatDay : Date.Day -> String
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


{-| A function that gets all the dates between two dates (inclusive).
-}
datesInRange : Date -> Date -> List Date
datesInRange min max =
    let
        go x acc =
            let
                y =
                    subDay x
            in
                if dateTuple y == dateTuple min then
                    y :: acc
                else
                    go y (y :: acc)
    in
        go (addDay max) []


{-| A function that converts a Date into a tuple. (year, month, day)
-}
dateTuple : Date -> ( Int, Int, Int )
dateTuple date =
    ( year date, monthToInt <| month date, day date )


{-| An opaque function that takes a function f, and an integer n, then repeats f, n times.
-}
repeat : (a -> a) -> Int -> a -> a
repeat f =
    let
        go n x =
            if n == 0 then
                x
            else
                go (n - 1) (f x)
    in
        go


{-| A function that takes a Date and returns the date representing the first of that month.
-}
startOfMonth : Date -> Date
startOfMonth date =
    mkDate (year date) (month date) 1


{-| A function that takes a Date and returns the date representing the end of that month.
-}
endOfMonth : Date -> Date
endOfMonth date =
    let
        y =
            year date

        m =
            month date
    in
        mkDate y m (daysInMonth y m)


{-| A function that returns a date given a starting date and the number of days to subtract from that starting date.

  - Ex. subDays 7 Date

-}
subDays : Int -> Date -> Date
subDays =
    repeat subDay


{-| A function that subtracts 1 day from the given date and returns it.
-}
subDay : Date -> Date
subDay date =
    let
        month =
            Date.month date

        year =
            Date.year date

        day =
            Date.day date - 1

        pred =
            predMonth month

        predYear =
            if pred == Dec then
                year - 1
            else
                year
    in
        if day < 1 then
            mkDate predYear pred (daysInMonth predYear pred)
        else
            mkDate year month day


{-| A function that returns a date given a starting date and the number of days to add to that starting date.

  - Ex. addDays 7 Date

-}
addDays : Int -> Date -> Date
addDays =
    repeat addDay


{-| A function that adds 1 day to the given date and returns it.
-}
addDay : Date -> Date
addDay date =
    let
        month =
            Date.month date

        year =
            Date.year date

        dim =
            daysInMonth year month

        day =
            Date.day date + 1

        succ =
            succMonth month

        succYear =
            if succ == Jan then
                year + 1
            else
                year
    in
        if day > dim then
            mkDate succYear succ 1
        else
            mkDate year month day


{-| A function that returns a date given a starting date and the number of months to add to that starting date.

  - Ex. addMonths 2 Date

-}
addMonths : Int -> Date -> Date
addMonths =
    repeat addMonth


{-| A function that adds 1 month from the given date and returns it.
-}
addMonth : Date -> Date
addMonth date =
    let
        month =
            Date.month date

        newMonth =
            monthFromInt <| monthToInt month + 1

        year =
            Date.year date

        newYear =
            if newMonth == Jan then
                year + 1
            else
                year

        day =
            Date.day date

        newDay =
            if day >= 29 && isLeapYear newYear then
                29
            else
                day
    in
        mkDate newYear newMonth newDay


{-| A function that returns a date given a starting date and the number of months to subtract from that starting date.

  - Ex. subMonths 2 Date

-}
subMonths : Int -> Date -> Date
subMonths =
    repeat subMonth


{-| A function that subtracts 1 month from the given date and returns it.
-}
subMonth : Date -> Date
subMonth date =
    let
        month =
            Date.month date

        newMonth =
            monthFromInt <| monthToInt month - 1

        year =
            Date.year date

        newYear =
            if newMonth == Dec then
                year - 1
            else
                year

        day =
            Date.day date

        newDay =
            if day >= 29 && isLeapYear newYear then
                29
            else
                day
    in
        mkDate newYear newMonth newDay


{-| A function that returns a date given a starting date and the number of years to subtract from that starting date.

  - Ex. subYears 2 Date

-}
subYears : Int -> Date -> Date
subYears =
    repeat subYear


{-| A function that subtracts 1 year from the given date and returns it.
-}
subYear : Date -> Date
subYear date =
    let
        month =
            Date.month date

        year =
            Date.year date - 1

        day =
            Date.day date
    in
        mkDate year month day


{-| A function that returns a date given a starting date and the number of years to add to that starting date.

  - Ex. addYears 2 Date

-}
addYears : Int -> Date -> Date
addYears =
    repeat addYear


{-| A function that adds 1 year to the given date and returns it.
-}
addYear : Date -> Date
addYear date =
    let
        month =
            Date.month date

        year =
            Date.year date + 1

        day =
            Date.day date
    in
        mkDate year month day


{-| A function that takes an Int for the day and returns a string, padding single digit days.

  - Ex. dayToString 2 -> "02"
  - Ex. dayToString 11 -> "11"

-}
dayToString : Int -> String
dayToString day =
    if day < 10 then
        "0" ++ toString day
    else
        toString day


{-| A function that takes a Day and returns it as an integer.

  - Sun - Sat
  - 1 - 7

-}
dayToInt : Date.Day -> Int
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


{-| A function that gets the day represented as an int and returns the day of the week.
-}
dayFromInt : Int -> Date.Day
dayFromInt day =
    case day of
        1 ->
            Sun

        2 ->
            Mon

        3 ->
            Tue

        4 ->
            Wed

        5 ->
            Thu

        6 ->
            Fri

        7 ->
            Sat

        _ ->
            Debug.crash ("dayFromInt: invalid day: " ++ toString day)


{-| A function that takes a Month and returns a string represented as a number, padding single digit months.

  - Ex. monthToString Jan -> "01"
  - Ex. monthToString Dec -> "12"

-}
monthToString : Month -> String
monthToString month =
    let
        int =
            monthToInt month
    in
        if int < 10 then
            "0" ++ toString int
        else
            toString int


{-| An opaque function returning the previous month.
-}
predMonth : Month -> Month
predMonth month =
    let
        prev =
            (monthToInt month - 1)
                |> flip rem 12
    in
        if prev == 0 then
            Dec
        else
            monthFromInt prev


{-| An opaque function returning the next month.
-}
succMonth : Month -> Month
succMonth month =
    monthToInt month
        |> flip rem 12
        |> (+) 1
        |> monthFromInt


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


{-| A function that takes an Int and returns the corresponding month.

  - Ex. 1 -> Jan
  - Ex. 12 -> Dec

-}
monthFromInt : Int -> Month
monthFromInt month =
    case month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        12 ->
            Dec

        x ->
            Debug.crash ("monthFromInt: invalid month: " ++ toString x)


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
isLeapYear y =
    y % 400 == 0 || y % 100 /= 0 && y % 4 == 0


{-| A function that takes a Year, Month, Day and returns a Date.
-}
mkDate : Year -> Month -> Day -> Date
mkDate year month day =
    toString year
        ++ "/"
        ++ monthToString month
        ++ "/"
        ++ dayToString day
        |> unsafeDate


{-| An opaque function that tries to parse a String into a Date.
-}
unsafeDate : String -> Date
unsafeDate date =
    case Date.fromString date of
        Err e ->
            Debug.crash ("unsafeDate: failed to parse date:" ++ e)

        Ok date ->
            date


{-| An opaque function that checks if date a is equal to date b
-}
($==) : Date -> Date -> Bool
($==) a b =
    Date.toTime a == Date.toTime b


{-| An opaque function that checks if date a is less than or equal to date b
-}
($<=) : Date -> Date -> Bool
($<=) a b =
    Date.toTime a <= Date.toTime b


{-| An opaque function that checks if date a is greater than or equal to date b
-}
($>=) : Date -> Date -> Bool
($>=) a b =
    Date.toTime a >= Date.toTime b


{-| An opaque function that checks if date a is less than to date b
-}
($<) : Date -> Date -> Bool
($<) a b =
    Date.toTime a < Date.toTime b


{-| An opaque function that checks if date a is greater than to date b
-}
($>) : Date -> Date -> Bool
($>) a b =
    Date.toTime a >= Date.toTime b
