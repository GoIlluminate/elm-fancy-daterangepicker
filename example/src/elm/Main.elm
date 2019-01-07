module Main exposing (Model, Msg(..), getSettings, init, main, printDate, printDateRange, update, view)

import Browser
import Date
    exposing
        ( Date
        )
import DatePicker exposing (getDate)
import DateRangePicker
    exposing
        ( defaultSettings
        , getDateRange
        , setCalendarDisplay
        , setInputIcon
        , setSettings
        )
import DateRangePicker.Common
    exposing
        ( CalendarDisplay(..)
        , DateRange
        , RestrictedDateRange(..)
        , calendarDisplayToDisplayStr
        )
import DateRangePicker.Date
    exposing
        ( formatDate
        )
import Html exposing (Html, div, h2, h4, i, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Time exposing (Month(..), Weekday(..))


type Msg
    = SetDateRangePicker DateRangePicker.Msg
    | SetDatePicker DatePicker.Msg
    | ChangeCalendarDisplay CalendarDisplay


type alias Model =
    { dateRange : Maybe DateRange
    , dateRangePicker : DateRangePicker.DateRangePicker
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    , calendarDisplay : CalendarDisplay
    , yearsInRange : Maybe Int
    , monthsInRange : Maybe Int
    , weeksInRange : Maybe Int
    , daysInRange : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            FullCalendar

        ( dateRangePicker_, dateRangePickerCmd ) =
            DateRangePicker.init

        ( datePicker_, datePickerCmd ) =
            DatePicker.init

        dateRangePicker =
            dateRangePicker_
                |> setSettings (getSettings True)
                |> DateRangePicker.setInputId "myDateRangePicker"
                |> setInputIcon (i [] [ text "📆" ])
                |> setCalendarDisplay calendarDisplay

        datePicker =
            datePicker_
                |> DatePicker.setSettings DatePicker.defaultSettings
                |> DatePicker.setCalendarDisplay calendarDisplay
    in
    ( { dateRange = Nothing
      , dateRangePicker = dateRangePicker
      , datePicker = datePicker
      , date = Nothing
      , calendarDisplay = calendarDisplay
      , yearsInRange = Nothing
      , monthsInRange = Nothing
      , weeksInRange = Nothing
      , daysInRange = Nothing
      }
    , Cmd.batch [ Cmd.map SetDateRangePicker dateRangePickerCmd, Cmd.map SetDatePicker datePickerCmd ]
    )


getSettings : Bool -> DateRangePicker.Settings
getSettings useDefault =
    if useDefault then
        DateRangePicker.defaultSettings

    else
        { defaultSettings
            | formatDateRange = DateRangePicker.formatDateRange
            , restrictedDateRange = ToPresent
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dateRangePicker, datePicker } as model) =
    case msg of
        SetDateRangePicker msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ dateRangePicker

                newDateRange =
                    getDateRange newDateRangePicker

                updatedModel =
                    case newDateRange of
                        Nothing ->
                            { model
                                | dateRangePicker = newDateRangePicker
                                , dateRange = newDateRange
                                , yearsInRange = Nothing
                                , monthsInRange = Nothing
                                , weeksInRange = Nothing
                                , daysInRange = Nothing
                            }

                        Just dr ->
                            { model
                                | dateRangePicker = newDateRangePicker
                                , dateRange = newDateRange
                                , yearsInRange = Just <| DateRangePicker.Common.yearsInRange dr
                                , monthsInRange = Just <| DateRangePicker.Common.monthsInRange dr
                                , weeksInRange = Just <| DateRangePicker.Common.weeksInRange dr
                                , daysInRange = Just <| DateRangePicker.Common.daysInRange dr
                            }
            in
            ( updatedModel
            , Cmd.map SetDateRangePicker dateRangePickerCmd
            )

        SetDatePicker msg_ ->
            let
                ( newDatePicker, datePickerCmd ) =
                    DatePicker.update msg_ datePicker
            in
            ( { model
                | datePicker = newDatePicker
                , date = getDate newDatePicker
              }
            , Cmd.map SetDatePicker datePickerCmd
            )

        ChangeCalendarDisplay calendarDisplay ->
            let
                newDateRangePicker =
                    DateRangePicker.setCalendarDisplay calendarDisplay dateRangePicker

                newDatePicker =
                    DatePicker.setCalendarDisplay calendarDisplay datePicker
            in
            ( { model
                | dateRangePicker = newDateRangePicker
                , datePicker = newDatePicker
                , calendarDisplay = calendarDisplay
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ calendarDisplayOptions model
        , dateRangePickers model
        , datePickers model
        ]


calendarDisplayOptions : Model -> Html Msg
calendarDisplayOptions model =
    let
        options =
            [ FullCalendar
            , ThreeMonths
            , TwoMonths
            , OneMonth
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
                , h4 [] [ text <| "Selected DateRange: " ++ printDateRange model.dateRange ]
                , DateRangePicker.view model.dateRangePicker |> Html.map SetDateRangePicker
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


datePickers : Model -> Html Msg
datePickers model =
    let
        dpView theme =
            div
                [ class "theme--wrapper"
                , class theme
                ]
                [ h2 [] [ text "Single Date Picker" ]
                , h4 [] [ text <| "Selected Date: " ++ printDate model.date ]
                , DatePicker.view model.datePicker |> Html.map SetDatePicker
                ]
    in
    div [ class "single-date-picker-wrapper date-picker--wrapper" ]
        [ dpView "theme-light"
        , dpView "theme-dark"
        ]


printDateRange : Maybe DateRange -> String
printDateRange dateRange =
    case dateRange of
        Just a ->
            DateRangePicker.formatDateRange a

        Nothing ->
            ""


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
    Sub.batch
        [ Sub.map SetDateRangePicker <| DateRangePicker.subscriptions model.dateRangePicker
        , Sub.map SetDatePicker <| DatePicker.subscriptions model.datePicker
        ]
