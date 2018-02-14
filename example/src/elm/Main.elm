module Main exposing (..)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import DateRangePicker exposing (defaultSettings)


type Msg
    = ToDateRangePicker DateRangePicker.Msg


type alias Model =
    { date : Maybe Date
    , dateRangePicker : DateRangePicker.DateRangePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dateRangePicker, dateRangePickerFx ) =
            DateRangePicker.init
    in
        { date = Nothing
        , dateRangePicker = dateRangePicker
        }
            ! [ Cmd.map ToDateRangePicker dateRangePickerFx ]


getSettings : Bool -> DateRangePicker.Settings
getSettings useDefault =
    if useDefault then
        DateRangePicker.defaultSettings
    else
        { defaultSettings | formatDateRange = formatDateRange }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, dateRangePicker } as model) =
    case msg of
        ToDateRangePicker msg ->
            let
                ( newDateRangePicker, dateRangePickerFx ) =
                    DateRangePicker.update (getSettings True) msg dateRangePicker
            in
                { model | dateRangePicker = newDateRangePicker } ! [ Cmd.map ToDateRangePicker dateRangePickerFx ]


view : Model -> Html Msg
view ({ date, dateRangePicker } as model) =
    div [ class "date-wrapper" ]
        [ DateRangePicker.view ( date, date ) DateRangePicker.defaultSettings dateRangePicker |> Html.map ToDateRangePicker
        ]


formatDateRange : DateRangePicker.DateRange -> String
formatDateRange dateRange =
    String.concat
        [ "["
        , toString dateRange.start
        , " TO "
        , toString dateRange.end
        , "]"
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
