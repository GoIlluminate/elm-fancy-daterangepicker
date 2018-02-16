module DateRangePicker.Common.Internal
    exposing
        ( FullYear
        , Quarter
        , EnabledDateRange
        , (?>)
        , ($!)
        , chunksOfLeft
        , inRange
        , prepareQuarters
        , prepareYear
        , padMonthLeft
        , padMonthRight
        , onPicker
        )

{-| A common internal library between DatePicker and DateRangePicker

@docs FullYear, Quarter

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import DateRangePicker.Common exposing (DateRange)
import DateRangePicker.Date exposing (datesInRange, mkDate, dayToInt)
import List.Extra as LE
import Html exposing (Html, div)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json


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


{-| An opaque function to check if a given date is within a
given dateRange.
-}
inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    let
        ( timeDate, timeStart, timeEnd ) =
            ( Date.toTime date
            , Date.toTime start
            , Date.toTime end
            )
    in
        if timeStart <= timeDate && timeEnd >= timeDate then
            True
        else
            False


{-| An opaque function that prepares the full year based on the given date.
-}
prepareYear : Date -> FullYear
prepareYear date =
    let
        yr =
            year date

        start =
            mkDate yr Jan 1

        end =
            mkDate yr Dec 31

        dates =
            datesInRange start end
    in
        { name = toString yr
        , year = yr
        , quarters =
            prepareQuarters <|
                LE.groupWhile (\x y -> (month x) == (month y)) dates
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
                        , toString <| idx + 1
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
            dayToInt <| dayOfWeek d

        n =
            dd - 1

        go =
            div [ Attrs.class "elm-daterangepicker--day-filler" ] []
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
            div [ Attrs.class "elm-daterangepicker--day-filler" ] []
    in
        List.repeat n go


{-| An opaque recursive function that chunks a list of a into a
list of lists of a of equal chunks.


## Example:

    chunksOfLeft 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] =
        [ [ 1, 2, 3 ]
        , [ 4.5, 6 ]
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


onPicker ev =
    Json.succeed
        >> Events.onWithOptions ev
            { preventDefault = False
            , stopPropagation = True
            }


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault


($!) : a -> List (Cmd b) -> ( a, Cmd b )
($!) model cmds =
    ( model, Cmd.batch cmds )
