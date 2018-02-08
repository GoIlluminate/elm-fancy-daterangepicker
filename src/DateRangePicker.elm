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
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td)
import Html.Attributes as Attrs exposing (class, colspan)
import Task
import DateRangePicker.Date exposing (initDate)


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
        something =
            ""
    in
        -- div [] [ text "hi" ]
        fullYearCalendar


dateRangePicker : ( Maybe Date, Maybe Date ) -> Settings -> Model -> Html Msg
dateRangePicker ( selectedStartDate, selectedEndDate ) settings ({ today } as model) =
    let
        something =
            ""
    in
        div [] []


fullYearCalendar : Html Msg
fullYearCalendar =
    div [ class "full-year-calendar-wrapper" ]
        [ div [ class "full-year-calendar" ]
            [ div [ class "yr-label" ] [ text "2018" ]
            , printQuarter "Q1" [ "January", "February", "March" ]
            , printQuarter "Q2" [ "April", "May", "June" ]
            , printQuarter "Q3" [ "July", "August", "September" ]
            , printQuarter "Q4" [ "October", "November", "December" ]
            ]
        ]


printQuarter : String -> List String -> Html Msg
printQuarter qtr months =
    div [ class "quarter-row" ] <|
        ([ div [ class "qtr-label" ] [ text qtr ] ]
            ++ List.map printMonth months
        )


printMonth : String -> Html Msg
printMonth month =
    div [ class "month" ]
        [ div [ class "month-label" ] [ text month ]
        , printWeek
        , printWeek
        , printWeek
        , printWeek
        ]


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


printMonthCalendar : String -> Html Msg
printMonthCalendar month =
    table [ class "month-table" ]
        [ thead []
            [ th [ colspan 7 ] [ text month ]
            ]
        , tbody []
            [ tr []
                [ td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                ]
            , tr []
                [ td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                ]
            , tr []
                [ td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                ]
            , tr []
                [ td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                ]
            , tr []
                [ td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                , td [ class "day" ] []
                ]
            ]
        ]


(!) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )
