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
import DateRangePicker.Date exposing (initDate, mkDate, datesInRangeIncl, dayToInt, dayFromInt, formatDay, formatDate, formatMonth, daysInMonth)


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = CurrentDate Date
    | SelectDate (Maybe Date)
    | PrevYear
    | NextYear


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
    , startDate : Maybe Date
    , endDate : Maybe Date
    , inputText : Maybe String
    , open : Bool
    , currentYear : FullYear
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
        , startDate = Nothing
        , endDate = Nothing
        , inputText = Nothing
        , open = False
        , currentYear = prepareYear initDate
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

        _ ->
            model ! []


{-| Expose if the daterange picker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : ( Maybe Date, Maybe Date ) -> Settings -> DateRangePicker -> Html Msg
view ( selectedStartDate, selectedEndDate ) settings (DateRangePicker ({ open, currentYear } as model)) =
    fullYearCalendar currentYear



-- div [] <| List.map (\d -> p [] [ text <| toString d ]) dates


dateRangePicker : ( Maybe Date, Maybe Date ) -> Settings -> Model -> Html Msg
dateRangePicker ( selectedStartDate, selectedEndDate ) settings ({ today } as model) =
    let
        something =
            ""
    in
        div [] []


fullYearCalendar : FullYear -> Html Msg
fullYearCalendar fullYear =
    let
        something =
            ""
    in
        div [ class "full-year-calendar-wrapper" ]
            [ div [ class "full-year-calendar" ] <|
                (printYearLabel fullYear
                    ++ List.map printQuarter fullYear.quarters
                )
            ]


printYearLabel : FullYear -> List (Html Msg)
printYearLabel fullYear =
    [ div [ class "yr-label-wrapper" ]
        [ div [ class "yr-btn yr-prev", onClick PrevYear ] [ text "<" ]
        , div [ class "yr-btn yr-label" ] [ text fullYear.name ]
        , div [ class "yr-btn yr-next", onClick NextYear ] [ text ">" ]
        ]
    ]


printQuarter : Quarter -> Html Msg
printQuarter qtr =
    div [ class "qtr-row" ] <|
        ([ div [ class "qtr-label" ] [ text qtr.name ] ]
            ++ List.map printMonth qtr.months
        )


printMonth : List Date -> Html Msg
printMonth m =
    let
        h =
            List.head m

        t =
            List.head <| List.reverse m
    in
        case ( h, t ) of
            ( Just a, Just b ) ->
                let
                    days =
                        [] ++ padDaysLeft a ++ List.map printDay m
                in
                    div [ class "month" ]
                        ([ div [ class "month-label" ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                         ]
                            ++ printDaysOfWeek
                            ++ days
                            ++ padMonth (42 - List.length days)
                        )

            ( _, _ ) ->
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


printDay : Date -> Html Msg
printDay date =
    div [ class "day" ] [ text <| toString <| day date ]


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
