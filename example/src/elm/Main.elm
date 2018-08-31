module Main exposing (..)

import Date
    exposing
        ( Date
        , Day(..)
        , Month(..)
        )
import Html exposing (Html, div, h2, h4, text, i)
import Html.Attributes exposing (class)
import DateRangePicker
    exposing
        ( defaultSettings
        , getDateRange
        , setSettings
        , setInputIcon
        )
import DateRangePicker.Date
    exposing
        ( formatDate
        )
import DatePicker exposing (getDate)
import DateRangePicker.Common
    exposing
        ( DateRange
        , RestrictedDateRange(ToPresent, FromPresent)
        )


type Msg
    = SetDateRangePicker DateRangePicker.Msg
    | SetDatePicker DatePicker.Msg


type alias Model =
    { dateRange : Maybe DateRange
    , dateRangePicker : DateRangePicker.DateRangePicker
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dateRangePicker_, dateRangePickerCmd ) =
            DateRangePicker.init

        ( datePicker_, datePickerCmd ) =
            DatePicker.init

        dateRangePicker =
            dateRangePicker_
                |> setSettings (getSettings True)
                |> DateRangePicker.setInputId "myDateRangePicker"
                |> setInputIcon (i [ class "fa fa-calendar" ] [])

        datePicker =
            datePicker_
    in
        { dateRange = Nothing
        , dateRangePicker = dateRangePicker
        , datePicker = datePicker
        , date = Nothing
        }
            ! [ Cmd.map SetDateRangePicker dateRangePickerCmd, Cmd.map SetDatePicker datePickerCmd ]


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
            in
                { model
                    | dateRangePicker = newDateRangePicker
                    , dateRange = getDateRange newDateRangePicker
                }
                    ! [ Cmd.map SetDateRangePicker dateRangePickerCmd ]

        SetDatePicker msg_ ->
            let
                ( newDatePicker, datePickerCmd ) =
                    DatePicker.update msg_ datePicker
            in
                { model
                    | datePicker = newDatePicker
                    , date = getDate newDatePicker
                }
                    ! [ Cmd.map SetDatePicker datePickerCmd ]


view : Model -> Html Msg
view { dateRange, dateRangePicker, datePicker, date } =
    div [ class "main" ]
        [ div [ class "date-range-picker-wrapper" ]
            [ h2 [] [ text "Date Range Picker" ]
            , h4 [] [ text <| "Selected DateRange: " ++ printDateRange dateRange ]
            , DateRangePicker.view dateRangePicker |> Html.map SetDateRangePicker
            ]
        , div [ class "single-date-picker-wrapper" ]
            [ h2 [] [ text "Single Date Picker" ]
            , h4 [] [ text <| "Selected Date: " ++ printDate date ]
            , DatePicker.view datePicker |> Html.map SetDatePicker
            ]
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
