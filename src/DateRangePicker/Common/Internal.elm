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
        , mkEnabledDateRangeFromRestrictedDateRange
        , mkClass
        , isDisabledDate
        , getDaysOfWeek
        )

{-| A common internal library between DatePicker and DateRangePicker

@docs FullYear, Quarter

-}

import List.Extra as LE
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import Html
    exposing
        ( Html
        , div
        , text
        )
import Date
    exposing
        ( Date
        , Day(..)
        , Month(..)
        , day
        , dayOfWeek
        , month
        , year
        )
import DateRangePicker.Common
    exposing
        ( DateRange
        , RestrictedDateRange(..)
        , mkDateRange
        )
import DateRangePicker.Date
    exposing
        ( datesInRange
        , mkDate
        , dayToInt
        , addDays
        , subDays
        , formatDay
        , dayFromInt
        , ($<)
        , ($>)
        )


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
                    date $< start

                ( Nothing, Just end ) ->
                    date $> (addDays 1 end)

                ( Nothing, Nothing ) ->
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
                    subDays 1 today
            in
                mkEnabledDateRange Nothing (Just yesterday)

        Future ->
            let
                tomorrow =
                    addDays 1 today
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
                { start = Just <| mkDate (year a) (month a) (day a)
                , end = Just <| mkDate (year b) (month b) (day b)
                }

        ( Just a, Nothing ) ->
            Just <|
                { start = Just <| mkDate (year a) (month a) (day a)
                , end = Nothing
                }

        ( Nothing, Just b ) ->
            Just <|
                { start = Nothing
                , end = Just <| mkDate (year b) (month b) (day b)
                }

        ( Nothing, Nothing ) ->
            Nothing


{-| An opaque function that gets the Days of the Week Html Msg for the calendar.
-}
getDaysOfWeek : List (Html msg)
getDaysOfWeek =
    let
        days =
            List.range 1 7

        go n =
            div [ Attrs.class "elm-daterangepicker--dow" ]
                [ text <|
                    formatDay <|
                        dayFromInt n
                ]
    in
        List.map go days


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


{-| An opaque function that returns a class name or an empty string
if the bool is true or not
-}
mkClass : String -> Bool -> String
mkClass cls bool =
    if bool then
        cls
    else
        ""


onPicker : String -> msg -> Html.Attribute msg
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
