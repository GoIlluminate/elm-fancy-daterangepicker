module Main exposing (..)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (DateEvent(..))
import Html exposing (Html, div, h1, text)
import DateRangePicker exposing (..)


type Msg
    = ToDatePicker DatePicker.Msg
    | ToDateRangePicker DateRangePicker.Msg


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , dateRangePicker : DateRangePicker.DateRangePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        ( dateRangePicker, dateRangePickerFx ) =
            DateRangePicker.init
    in
        { date = Nothing
        , datePicker = datePicker
        , dateRangePicker = dateRangePicker
        }
            ! [ Cmd.map ToDatePicker datePickerFx, Cmd.map ToDateRangePicker dateRangePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker, dateRangePicker } as model) =
    case msg of
        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update DatePicker.defaultSettings msg datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            date
            in
                { model
                    | date = newDate
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePicker datePickerFx ]

        ToDateRangePicker msg ->
            let
                ( newDateRangePicker, dateRangePickerFx ) =
                    DateRangePicker.update DateRangePicker.defaultSettings msg dateRangePicker
            in
                { model | dateRangePicker = newDateRangePicker } ! [ Cmd.map ToDateRangePicker dateRangePickerFx ]


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    div []
        [ case date of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just date ->
                h1 [] [ text <| formatDate date ]
        , DatePicker.view date DatePicker.defaultSettings datePicker
            |> Html.map ToDatePicker
        ]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
