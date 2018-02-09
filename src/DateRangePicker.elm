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

import Date exposing (Date, Day(..), Month(..), day, month, year)
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td, p)
import Html.Attributes as Attrs exposing (class, colspan)
import Task
import List.Extra as LE
import DateRangePicker.Date exposing (initDate, mkDate, datesInRangeIncl, formatDate, formatMonth, daysInMonth)


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = CurrentDate Date
    | SelectDate (Maybe Date)


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
        }
    , Task.perform CurrentDate Date.now
    )


update : Settings -> Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update settings msg (DateRangePicker model) =
    case msg of
        CurrentDate date ->
            { model | today = date } ! []

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
view ( selectedStartDate, selectedEndDate ) settings (DateRangePicker ({ open } as model)) =
    let
        fullYear =
            prepareYear model.today
    in
        fullYearCalendar fullYear



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
                ([ div [ class "yr-label" ] [ text fullYear.name ] ]
                    ++ List.map printQuarter fullYear.quarters
                )
            ]


printQuarter : Quarter -> Html Msg
printQuarter qtr =
    div [ class "qtr-row" ] <|
        ([ div [ class "qtr-label" ] [ text qtr.name ] ]
            ++ List.map printMonth qtr.months
        )



-- printQuarter : String -> List String -> Html Msg
-- printQuarter qtr months =
--     div [ class "quarter-row" ] <|
--         ([ div [ class "qtr-label" ] [ text qtr ] ]
--             ++ List.map printMonth months
--         )


printMonth : List Date -> Html Msg
printMonth m =
    let
        h =
            List.head m
    in
        case h of
            Just a ->
                div [ class "month" ]
                    [ div [ class "month-label" ]
                        [ text <|
                            formatMonth <|
                                month a
                        ]
                    , printWeek
                    , printWeek
                    , printWeek
                    , printWeek
                    , printWeek
                    ]

            Nothing ->
                div [] []


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
    , quarters : List Quarter
    }
