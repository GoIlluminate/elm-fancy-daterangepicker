module DateRangePicker
    exposing
        ( Msg
        , DateRangePicker
        , init
        , update
        , defaultSettings
        , isOpen
        , view
        )

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td, p)
import Html.Attributes as Attrs exposing (class, colspan)
import Html.Events exposing (onClick)
import Task
import List.Extra as LE
import DateRangePicker.Date exposing (initDate, mkDate, startOfMonth, endOfMonth, datesInRangeIncl, dayToInt, dayFromInt, formatDay, formatDate, formatMonth, daysInMonth)


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = CurrentDate Date
    | PrevYear
    | NextYear
    | SetDateRange DateRange
    | DoNothing


type alias DateRange =
    { start : Date
    , end : Date
    }


{-| The settings that the DateRangePicker uses
-}
type alias Settings =
    { placeholder : String
    , className : Maybe String
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    }


{-| The model to be used within the DateRangePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , currentYear : FullYear
    , dateRange : Maybe DateRange
    }


{-| The DateRangePicker model.
-}
type DateRangePicker
    = DateRangePicker Model


defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , className = Just "elm-daterangepicker"
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    }


init : ( DateRangePicker, Cmd Msg )
init =
    ( DateRangePicker <|
        { today = initDate
        , inputText = Nothing
        , open = False
        , currentYear = prepareYear initDate
        , dateRange = Nothing
        }
    , Task.perform CurrentDate Date.now
    )


update : Settings -> Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update settings msg (DateRangePicker model) =
    case msg of
        CurrentDate date ->
            { model | today = date, currentYear = prepareYear date } ! []

        PrevYear ->
            let
                prevYear =
                    prepareYear <|
                        mkDate (model.currentYear.year - 1) Jan 1
            in
                { model | currentYear = prevYear } ! []

        NextYear ->
            let
                nextYear =
                    prepareYear <|
                        mkDate (model.currentYear.year + 1) Jan 1
            in
                { model | currentYear = nextYear } ! []

        SetDateRange dateRange ->
            { model | dateRange = Just dateRange } ! []

        DoNothing ->
            model ! []


{-| Expose if the daterange picker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : ( Maybe Date, Maybe Date ) -> Settings -> DateRangePicker -> Html Msg
view ( selectedStartDate, selectedEndDate ) settings (DateRangePicker model) =
    fullYearCalendar model



-- div [] <| List.map (\d -> p [] [ text <| toString d ]) dates


dateRangePicker : ( Maybe Date, Maybe Date ) -> Settings -> Model -> Html Msg
dateRangePicker ( selectedStartDate, selectedEndDate ) settings ({ today } as model) =
    let
        something =
            ""
    in
        div [] []


fullYearCalendar : Model -> Html Msg
fullYearCalendar model =
    div [ class "full-year-calendar-wrapper" ]
        [ div [ class "full-year-calendar" ] <|
            (printYearLabel model.currentYear
                ++ printQuarters model
            )
        ]


printYearLabel : FullYear -> List (Html Msg)
printYearLabel fullYear =
    let
        start =
            mkDate fullYear.year Jan 1

        end =
            mkDate fullYear.year Dec 31

        setYearRange =
            onClick <|
                SetDateRange <|
                    { start = start
                    , end = end
                    }
    in
        [ div [ class "yr-label-wrapper" ]
            [ div [ class "yr-btn yr-prev", onClick PrevYear ] [ text "<" ]
            , div [ class "yr-btn yr-label", setYearRange ] [ text fullYear.name ]
            , div [ class "yr-btn yr-next", onClick NextYear ] [ text ">" ]
            ]
        ]


printQuarters : Model -> List (Html Msg)
printQuarters model =
    let
        quarters =
            model.currentYear.quarters
    in
        List.map (printQuarter model) quarters


printQuarter : Model -> Quarter -> Html Msg
printQuarter model qtr =
    let
        m1 =
            List.head qtr.months

        m2 =
            List.head <|
                List.reverse qtr.months
    in
        case ( m1, m2 ) of
            ( Just a, Just b ) ->
                let
                    startOfQuarter =
                        List.head a

                    endOfQuarter =
                        List.head <|
                            List.reverse b

                    setQuarterDateRange =
                        case ( startOfQuarter, endOfQuarter ) of
                            ( Just aa, Just bb ) ->
                                let
                                    x =
                                        Debug.log "a" aa
                                in
                                    onClick <|
                                        SetDateRange <|
                                            { start = aa
                                            , end = bb
                                            }

                            ( _, _ ) ->
                                onClick DoNothing

                    monthDiv =
                        div [ class "qtr-row" ] <|
                            ([ div [ class "qtr-label", setQuarterDateRange ] [ text qtr.name ] ]
                                ++ List.map (printMonth model) qtr.months
                            )
                in
                    monthDiv

            ( _, _ ) ->
                div [] []


printMonth : Model -> List Date -> Html Msg
printMonth model m =
    let
        h =
            List.head m
    in
        case h of
            Just a ->
                let
                    days =
                        [] ++ padDaysLeft a ++ List.map (printDay model) m

                    setMonthDateRange =
                        onClick <|
                            SetDateRange <|
                                { start = startOfMonth a
                                , end = endOfMonth a
                                }

                    xxx =
                        Debug.log "startOfMonth" (startOfMonth a)

                    yyy =
                        Debug.log "endOfMonth" (endOfMonth a)

                    monthDiv =
                        div [ class "month-label", setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ class "month" ]
                        ([ monthDiv ]
                            ++ printDaysOfWeek
                            ++ days
                            ++ padMonth (42 - List.length days)
                        )

            _ ->
                div [] []


printDaysOfWeek : List (Html Msg)
printDaysOfWeek =
    let
        days =
            List.range 1 7

        go n =
            div [ class "dow" ] [ text <| formatDay <| dayFromInt n ]
    in
        List.map go days


padDaysLeft : Date -> List (Html Msg)
padDaysLeft d =
    let
        dd =
            dayToInt <| dayOfWeek d

        go =
            div [ class "day-filler" ] []
    in
        List.repeat (dd - 1) go


padDaysRight : Date -> List (Html Msg)
padDaysRight d =
    let
        dd =
            dayToInt <| dayOfWeek d

        go =
            div [ class "day-filler" ] []
    in
        List.repeat (7 - dd) go


padMonth : Int -> List (Html Msg)
padMonth i =
    List.repeat i <| div [ class "day-filler" ] []


printDay : Model -> Date -> Html Msg
printDay model date =
    let
        className =
            case model.dateRange of
                Just a ->
                    if inRange date a then
                        "day selected-range"
                    else
                        "day"

                Nothing ->
                    "day"
    in
        div [ class className ] [ text <| toString <| day date ]


printWeek : Html Msg
printWeek =
    div [ class "week" ]
        [ div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        , div [ class "day" ] []
        ]


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
            datesInRangeIncl start end
    in
        { name = toString yr
        , year = yr
        , quarters =
            prepareQuarters <|
                LE.groupWhile (\x y -> (month x) == (month y)) dates
        }


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


prepareQuarters : List (List Date) -> List Quarter
prepareQuarters lst =
    let
        qs =
            chunksOfLeft 3 lst
    in
        List.indexedMap (\idx q -> { name = "Q" ++ (toString (idx + 1)), months = q }) qs


(!) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )


type alias Quarter =
    { name : String
    , months : List (List Date)
    }


type alias FullYear =
    { name : String
    , year : Int
    , quarters : List Quarter
    }


inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    let
        ( timeDate, timeStart, timeEnd ) =
            ( Date.toTime date, Date.toTime start, Date.toTime end )
    in
        if timeStart <= timeDate && timeEnd >= timeDate then
            True
        else
            False
