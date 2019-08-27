module Main exposing (..)

import Browser
import Date
    exposing
        ( Date
        )
import DateRangePicker.Helper exposing (formatDate)
import DateRangePicker.Types exposing (DateRange)
import DateRangeSelector exposing (englishLanugageConfig, initModelWithOptions, openDateRangePicker)
import Html exposing (Html, button, div, h2, h4, i, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Task
import Time exposing (Month(..), Posix, Zone)


type Msg
    = ChangeCalendarDisplay DateRangeSelector.CalendarType
    | NewTime Posix
    | NewZone Zone
    | NewSelectorMsgs DateRangeSelector.Msg


type alias Model =
    { dateRange : Maybe DateRange
    , date : Maybe Date
    , calendarDisplay : DateRangeSelector.CalendarType
    , yearsInRange : Maybe Int
    , monthsInRange : Maybe Int
    , weeksInRange : Maybe Int
    , daysInRange : Maybe Int
    , dateSelector : DateRangeSelector.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            DateRangeSelector.FullCalendar

    in
    ( { dateRange = Nothing
      , date = Nothing
      , calendarDisplay = calendarDisplay
      , yearsInRange = Nothing
      , monthsInRange = Nothing
      , weeksInRange = Nothing
      , daysInRange = Nothing
      , dateSelector =
            initModelWithOptions
                { availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
                , availableForSelectionEnd = Date.fromCalendarDate 2100 Jan 1
                , presets =
                    [ DateRangeSelector.Today
                    , DateRangeSelector.Yesterday
                    , DateRangeSelector.PastWeek
                    , DateRangeSelector.PastMonth
                    , DateRangeSelector.PastYear
                    ]
                , calendarType = calendarDisplay
                , isOpen = False
                , languageConfig = englishLanugageConfig
                }
      , today = Nothing
      , zone = Nothing
      }
    , Cmd.batch
        [ Task.perform NewTime Time.now
        , Task.perform NewZone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( model) =
    case msg of
        ChangeCalendarDisplay calendarDisplay ->
            let
                selector = 
                    model.dateSelector
                newDateRangeSelector =
                    {selector | calendarType = calendarDisplay}
            in
            ( { model
                | calendarDisplay = calendarDisplay
                , dateSelector = newDateRangeSelector
              }
            , Cmd.none
            )

        NewTime posix ->
            ( { model | today = Just posix }, Cmd.none )

        NewSelectorMsgs msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangeSelector.update msg_ model.dateSelector
            in
            ( { model | dateSelector = newDateRangePicker }, Cmd.map NewSelectorMsgs dateRangePickerCmd )

        NewZone zone ->
            ( { model | zone = Just zone }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        selector =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class "theme-light open-button" ]
                        [ button [ openDateRangePicker ] [ text "Open Me!" ]
                        , DateRangeSelector.view t z model.dateSelector
                        ]

                _ ->
                    text ""
    in
    div [ class "main" ]
        [ calendarDisplayOptions model
        , Html.map NewSelectorMsgs selector
        , dateRangePickers model
        ]


calendarDisplayOptions : Model -> Html Msg
calendarDisplayOptions model =
    let
        options =
            [ DateRangeSelector.FullCalendar
            , DateRangeSelector.ThreeMonths
            , DateRangeSelector.TwoMonths
            , DateRangeSelector.OneMonth
            ]

        selectedClass calendarDisplay =
            if calendarDisplay == model.calendarDisplay then
                "selected"

            else
                ""

        go calendarDisplay =
            div
                [ class "calendar-display-option"
                , class <| selectedClass calendarDisplay
                , Html.Events.onClick <| ChangeCalendarDisplay calendarDisplay
                ]
                [ span [] [ text <| calendarDisplayToDisplayStr calendarDisplay ]
                ]
    in
    div [ class "calendar-display-options--container" ]
        [ div [ class "calendar-display-options--wrapper" ] <| List.map go options
        ]


dateRangePickers : Model -> Html Msg
dateRangePickers model =
    let
        numInRangeView str maybeN =
            case maybeN of
                Nothing ->
                    text ""

                Just n ->
                    div [] [ text <| str ++ " " ++ String.fromInt n ]

        drpView theme =
            div
                [ class "theme--wrapper"
                , class theme
                ]
                [ h2 [] [ text "Date Range Picker" ]
                , div [ class "in-range--container" ]
                    [ numInRangeView "Years in DateRange:" model.yearsInRange
                    , numInRangeView "Months in DateRange:" model.monthsInRange
                    , numInRangeView "Weeks in DateRange:" model.weeksInRange
                    , numInRangeView "Days in DateRange:" model.daysInRange
                    ]
                ]
    in
    div [ class "date-range-picker-wrapper date-picker--wrapper" ]
        [ drpView "theme-light"
        , drpView "theme-dark"
        ]


printDate : Maybe Date -> String
printDate date =
    case date of
        Just a ->
            formatDate a

        Nothing ->
            ""


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        selectorSubscriptions =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    DateRangeSelector.subscriptions model.dateSelector t z

                _ ->
                    Sub.none
    in
    Sub.batch
        [  Sub.map NewSelectorMsgs <| selectorSubscriptions
        ]


calendarDisplayToDisplayStr : DateRangeSelector.CalendarType -> String
calendarDisplayToDisplayStr calendarDisplay =
    case calendarDisplay of
        DateRangeSelector.FullCalendar ->
            "FullCalendar"

        DateRangeSelector.ThreeMonths ->
            "ThreeMonths"

        DateRangeSelector.TwoMonths ->
            "TwoMonths"

        DateRangeSelector.OneMonth ->
            "OneMonth"
