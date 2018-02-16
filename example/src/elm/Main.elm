module Main exposing (..)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import DateRangePicker exposing (defaultSettings, getDateRange, setSettings, setDateRange)
import DateRangePicker.Date exposing (mkDate, monthToInt)
import DatePicker
import DateRangePicker.Common exposing (DateRange, mkDateRange, RestrictedDateRange(Off, ToPresent, FromPresent, Past, Future, Between, To, From))


type Msg
    = ToDateRangePicker DateRangePicker.Msg
    | ToDatePicker DatePicker.Msg


type alias Model =
    { dateRange : Maybe DateRange
    , dateRangePicker : DateRangePicker.DateRangePicker
    , datePicker : DatePicker.DatePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dateRangePicker_, dateRangePickerFx ) =
            DateRangePicker.init

        ( datePicker_, datePickerFx ) =
            DatePicker.init

        dateRangePicker =
            dateRangePicker_
                |> setSettings (getSettings True)

        datePicker =
            datePicker_
    in
        { dateRange = Nothing
        , dateRangePicker = dateRangePicker
        , datePicker = datePicker
        }
            ! [ Cmd.map ToDateRangePicker dateRangePickerFx, Cmd.map ToDatePicker datePickerFx ]


getSettings : Bool -> DateRangePicker.Settings
getSettings useDefault =
    if useDefault then
        DateRangePicker.defaultSettings
    else
        { defaultSettings
            | formatDateRange = formatDateRange
            , restrictedDateRange = ToPresent
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dateRange, dateRangePicker, datePicker } as model) =
    case msg of
        ToDateRangePicker msg ->
            let
                ( newDateRangePicker, dateRangePickerFx ) =
                    DateRangePicker.update msg dateRangePicker
            in
                { model
                    | dateRangePicker = newDateRangePicker
                    , dateRange = getDateRange newDateRangePicker
                }
                    ! [ Cmd.map ToDateRangePicker dateRangePickerFx ]

        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx ) =
                    DatePicker.update msg datePicker
            in
                { model
                    | datePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePicker datePickerFx ]


view : Model -> Html Msg
view ({ dateRange, dateRangePicker, datePicker } as model) =
    div [ class "main" ]
        [ div [ class "date-range-picker-wrapper" ]
            [ div [] [ text "Date Range Picker" ]
            , DateRangePicker.view dateRangePicker |> Html.map ToDateRangePicker
            ]
        , div [ class "single-date-picker-wrapper" ]
            [ div [] [ text "Single Date Picker" ]
            , DatePicker.view datePicker |> Html.map ToDatePicker
            ]
        ]


printDateRange : Maybe DateRange -> String
printDateRange dateRange =
    ""



-- case dateRange of
--     Nothing ->
--         "No date selected."
--     Just a ->
--         formatDateRange2 a


formatDateRange : DateRange -> String
formatDateRange dateRange =
    String.concat
        [ "[ "
        , toString <| monthToInt <| month dateRange.start
        , "/"
        , toString <| day dateRange.start
        , "/"
        , toString <| year dateRange.start
        , " TO "
        , toString <| monthToInt <| month dateRange.end
        , "/"
        , toString <| day dateRange.end
        , "/"
        , toString <| year dateRange.end
        , " ]"
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
