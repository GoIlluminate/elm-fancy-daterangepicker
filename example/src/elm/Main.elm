module Main exposing (..)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import DateRangePicker exposing (defaultSettings, DateRange, getDateRange, setSettings, setDateRange, mkDateRange, RestrictedDateRange(Off, ToPresent, FromPresent, Past, Future, Between, To, From))
import DateRangePicker.Date exposing (mkDate, monthToInt)


type Msg
    = ToDateRangePicker DateRangePicker.Msg


type alias Model =
    { dateRange : Maybe DateRange
    , dateRangePicker : DateRangePicker.DateRangePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( dateRangePicker_, dateRangePickerFx ) =
            DateRangePicker.init

        dateRangePicker =
            dateRangePicker_
                |> setDateRange
                    (Just <|
                        mkDateRange (mkDate 2017 Jan 1) (mkDate 2017 Feb 2)
                    )
                |> setSettings (getSettings True)
    in
        { dateRange = Nothing
        , dateRangePicker = dateRangePicker
        }
            ! [ Cmd.map ToDateRangePicker dateRangePickerFx ]


getSettings : Bool -> DateRangePicker.Settings
getSettings useDefault =
    if useDefault then
        DateRangePicker.defaultSettings
    else
        { defaultSettings
            | formatDateRange = formatDateRange
            , restrictedDateRange = Off
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dateRange, dateRangePicker } as model) =
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


view : Model -> Html Msg
view ({ dateRange, dateRangePicker } as model) =
    div [ class "date-wrapper" ]
        [ div [] [ text <| printDateRange dateRange ]
        , DateRangePicker.view dateRangePicker |> Html.map ToDateRangePicker
        ]


printDateRange : Maybe DateRange -> String
printDateRange dateRange =
    case dateRange of
        Nothing ->
            "No date selected."

        Just a ->
            formatDateRange a


formatDateRange : DateRangePicker.DateRange -> String
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
