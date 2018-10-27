module DateRangePicker.Common.Internal exposing
    ( CalendarRange
    , EnabledDateRange
    , FullYear
    , Months
    , Quarter
    , chunksOfLeft
    , isDisabledDate
    , mkClass
    , mkClassString
    , mkEnabledDateRangeFromRestrictedDateRange
    , noPresets
    , onClickNoDefault
    , padMonthLeft
    , padMonthRight
    , prepareCalendarRange
    , prepareQuarters
    , prepareYear
    , renderDaysOfWeek
    )

{-| A common internal library between DatePicker and DateRangePicker
-}

import Date
    exposing
        ( Date
        , day
        , fromCalendarDate
        , month
        , numberToWeekday
        , year
        )
import DateRangePicker.Common
    exposing
        ( CalendarDisplay(..)
        , RestrictedDateRange(..)
        , inRange
        , mkDateRange
        )
import DateRangePicker.Date
    exposing
        ( dateGreaterThan
        , dateLessThan
        , dayToInt
        , endOfMonth
        , endOfQuarter
        , formatDay
        , formatMonth
        , monthAbbr
        , startOfMonth
        , startOfQuarter
        )
import Html
    exposing
        ( Html
        , div
        , span
        , text
        )
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import List.Extra as LE
import Time exposing (Month(..), Weekday(..))


{-| An opaque type to represent the displayed calendar range that the daterange picker is using.
-}
type alias CalendarRange =
    { name : String
    , start : Date
    , end : Date
    , months : Months
    }


{-| An opaque type to represent a list of a list of dates (months).
-}
type alias Months =
    List (List Date)


{-| An opaque type to represent the full year that the daterangepicker is using.
-}
type alias FullYear =
    { name : String
    , year : Int
    , quarters : List Quarter
    }


{-| An opaque type representing a quarter within the FullYear. Ex. (Jan, Feb, March) represents Q1.
-}
type alias Quarter =
    { name : String
    , months : List (List Date)
    }


{-| An opaque type representing the enabled dates for the datepicker
-}
type alias EnabledDateRange =
    { start : Maybe Date
    , end : Maybe Date
    }


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
                    not <| inRange date <| mkDateRange start end

                ( Just start, Nothing ) ->
                    dateLessThan date start

                ( Nothing, Just end ) ->
                    dateGreaterThan date end

                ( Nothing, Nothing ) ->
                    False


{-| An opaque function that prepares the full year based on the given date.
-}
prepareCalendarRange : CalendarDisplay -> Date -> CalendarRange
prepareCalendarRange calendarDisplay date =
    let
        yr =
            year date

        ( start, end ) =
            case calendarDisplay of
                FullCalendar ->
                    ( fromCalendarDate yr Jan 1
                    , fromCalendarDate yr Dec 31
                    )

                ThreeMonths ->
                    ( startOfQuarter date, endOfQuarter date )

                TwoMonths ->
                    ( startOfMonth date, endOfMonth <| Date.add Date.Months 1 date )

                OneMonth ->
                    ( startOfMonth date, endOfMonth date )

        dates =
            Date.range Date.Day 1 start (Date.add Date.Days 1 end)

        name =
            case calendarDisplay of
                FullCalendar ->
                    String.fromInt yr

                ThreeMonths ->
                    String.join " - "
                        [ String.join " " [ monthAbbr <| Date.month start, String.fromInt yr ]
                        , String.join " " [ monthAbbr <| Date.month end, String.fromInt yr ]
                        ]

                TwoMonths ->
                    String.join " - "
                        [ String.join " " [ monthAbbr <| Date.month start, String.fromInt yr ]
                        , String.join " " [ monthAbbr <| Date.month end, String.fromInt yr ]
                        ]

                OneMonth ->
                    String.join " " [ formatMonth <| Date.month start, String.fromInt yr ]

        months =
            List.map (\x -> Tuple.first x :: Tuple.second x) <|
                LE.groupWhile (\x y -> month x == month y) dates
    in
    CalendarRange name start end months


{-| An opaque function that prepares the full year based on the given date.
-}
prepareYear : Date -> FullYear
prepareYear date =
    let
        yr =
            year date

        start =
            fromCalendarDate yr Jan 1

        end =
            fromCalendarDate (yr + 1) Jan 1

        dates =
            Date.range Date.Day 1 start end
    in
    { name = String.fromInt yr
    , year = yr
    , quarters =
        prepareQuarters <|
            List.map (\x -> Tuple.first x :: Tuple.second x) <|
                LE.groupWhile (\x y -> month x == month y) dates
    }


{-| An opaque function that prepares the quarters of the full year
given the full list of dates for the year.
-}
prepareQuarters : List (List Date) -> List Quarter
prepareQuarters lst =
    let
        qs =
            chunksOfLeft 3 lst
    in
    List.indexedMap
        (\idx q ->
            { name =
                String.concat
                    [ "Q"
                    , String.fromInt <| idx + 1
                    ]
            , months = q
            }
        )
        qs


{-| An opaque function taht pads the month from the left with filler days
in order to get the first of the month to line up correctly with the correct
day of the week.
-}
padMonthLeft : Date -> List (Html msg)
padMonthLeft d =
    let
        dd =
            dayToInt <| Date.weekday d

        n =
            dd - 1

        go =
            div [ Attrs.class "elm-fancy-daterangepicker--day-filler" ] []
    in
    List.repeat n go


{-| An opaque function that pads the end of the month with filler days in order to fill
the month with 42 total days (days + filler days) to keep the size of each month in the
calendar the same size.
-}
padMonthRight : Int -> List (Html msg)
padMonthRight n =
    let
        go =
            div [ Attrs.class "elm-fancy-daterangepicker--day-filler" ] []
    in
    List.repeat n go


{-| An opaque function that makes the EnabledDateRange from settings.


## EnabledDateRange is Nothing if RestrictedDateRange is Off

-}
mkEnabledDateRangeFromRestrictedDateRange : RestrictedDateRange -> Date -> Maybe EnabledDateRange
mkEnabledDateRangeFromRestrictedDateRange restrictedDateRange today =
    case restrictedDateRange of
        Off ->
            mkEnabledDateRange Nothing Nothing

        ToPresent ->
            mkEnabledDateRange Nothing (Just today)

        FromPresent ->
            mkEnabledDateRange (Just today) Nothing

        Past ->
            let
                yesterday =
                    Date.add Date.Days -1 today
            in
            mkEnabledDateRange Nothing (Just yesterday)

        Future ->
            let
                tomorrow =
                    Date.add Date.Days 1 today
            in
            mkEnabledDateRange (Just tomorrow) Nothing

        Between start end ->
            mkEnabledDateRange (Just start) (Just end)

        To date ->
            mkEnabledDateRange Nothing (Just date)

        From date ->
            mkEnabledDateRange (Just date) Nothing


{-| An opaque function that makes an EnabledDateRange from two Maybe Dates
-}
mkEnabledDateRange : Maybe Date -> Maybe Date -> Maybe EnabledDateRange
mkEnabledDateRange start end =
    case ( start, end ) of
        ( Just a, Just b ) ->
            Just <|
                { start = Just <| fromCalendarDate (year a) (month a) (day a)
                , end = Just <| fromCalendarDate (year b) (month b) (day b)
                }

        ( Just a, Nothing ) ->
            Just <|
                { start = Just <| fromCalendarDate (year a) (month a) (day a)
                , end = Nothing
                }

        ( Nothing, Just b ) ->
            Just <|
                { start = Nothing
                , end = Just <| fromCalendarDate (year b) (month b) (day b)
                }

        ( Nothing, Nothing ) ->
            Nothing


{-| An opaque function that gets the Days of the Week Html Msg for the calendar.
-}
renderDaysOfWeek : List (Html msg)
renderDaysOfWeek =
    let
        days =
            7 :: List.range 1 6

        go n =
            div [ Attrs.class "elm-fancy-daterangepicker--dow" ]
                [ text <|
                    formatDay <|
                        numberToWeekday n
                ]
    in
    List.map go days


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


{-| An opaque function that returns a class name or an empty string
if the bool is true or not
-}
mkClass : String -> Bool -> String
mkClass cls bool =
    if bool then
        cls

    else
        ""


{-| An opaque function that returns a class string from a list of string.
-}
mkClassString : List String -> String
mkClassString lst =
    String.join " " <|
        List.filter (\x -> x /= "")
            lst


{-| An opaque function that stop propagation of click event.
-}
onClickNoDefault : msg -> Html.Attribute msg
onClickNoDefault message =
    Events.custom "click" <|
        Json.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }


{-| An opaque function that returns a no presets available Html msg
-}
noPresets : List (Html msg)
noPresets =
    [ div [ Attrs.class "elm-fancy-daterangepicker--preset elm-fancy-daterangepicker--no-presets" ]
        [ span [ Attrs.class "elm-fancy-daterangepicker--preset-name" ] [ text "No presets available..." ]
        , span [ Attrs.class "elm-fancy-daterangepicker--preset-value" ] []
        ]
    ]
