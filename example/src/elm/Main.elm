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
        )
import DateRangePicker.Date
    exposing
        ( formatDate
        )
import Html exposing (Html, div, h2, h4, i, text)
import Html.Attributes exposing (class)
import Time exposing (Month(..), Weekday(..))


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
                |> setInputIcon (i [] [ text "📆" ])
                |> setCalendarDisplay ThreeMonths

        datePicker =
            datePicker_
                |> DatePicker.setSettings DatePicker.defaultSettings
                |> DatePicker.setCalendarDisplay ThreeMonths
    in
    ( { dateRange = Nothing
      , dateRangePicker = dateRangePicker
      , datePicker = datePicker
      , date = Nothing
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
            in
            ( { model
                | dateRangePicker = newDateRangePicker
                , dateRange = getDateRange newDateRangePicker
              }
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


view : Model -> Html Msg
view { dateRange, dateRangePicker, datePicker, date } =
    div [ class "main" ]
        [ div [ class "date-range-picker-wrapper date-picker--wrapper" ]
            [ div [ class "theme--wrapper theme-light" ]
                [ h2 [] [ text "Date Range Picker" ]
                , h4 [] [ text <| "Selected DateRange: " ++ printDateRange dateRange ]
                , DateRangePicker.view dateRangePicker |> Html.map SetDateRangePicker
                ]
            , div [ class "theme--wrapper theme-dark" ]
                [ h2 [] [ text "Date Range Picker" ]
                , h4 [] [ text <| "Selected DateRange: " ++ printDateRange dateRange ]
                , DateRangePicker.view dateRangePicker |> Html.map SetDateRangePicker
                ]
            ]
        , div [ class "single-date-picker-wrapper date-picker--wrapper" ]
            [ div [ class "theme--wrapper theme-light" ]
                [ h2 [] [ text "Single Date Picker" ]
                , h4 [] [ text <| "Selected Date: " ++ printDate date ]
                , DatePicker.view datePicker |> Html.map SetDatePicker
                ]
            , div [ class "theme--wrapper theme-dark" ]
                [ h2 [] [ text "Single Date Picker" ]
                , h4 [] [ text <| "Selected Date: " ++ printDate date ]
                , DatePicker.view datePicker |> Html.map SetDatePicker
                ]
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
