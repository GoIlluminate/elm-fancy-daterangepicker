module Main exposing (Model, Msg(..), calendarDisplayOptions, calendarDisplayToDisplayStr, init, main, subscriptions, update, view)

import Browser
import Date
    exposing
        ( Date
        )
import DateRangePicker exposing (englishLanguageConfig, initModelWithOptions, openDateRangePicker)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Task
import Time exposing (Month(..), Posix, Zone)


type Msg
    = ChangeCalendarDisplay DateRangePicker.CalendarType
    | NewTime Posix
    | NewZone Zone
    | DatePickerMsgs DateRangePicker.Msg
    | ToggleColorTheme


type ColorTheme
    = Light
    | Dark


type alias Model =
    { calendarDisplay : DateRangePicker.CalendarType
    , datePicker : DateRangePicker.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    , colorTheme : ColorTheme
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            DateRangePicker.FullCalendar
    in
    ( { calendarDisplay = calendarDisplay
      , datePicker =
            initModelWithOptions
                { availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
                , availableForSelectionEnd = Date.fromCalendarDate 2100 Jan 1
                , presets =
                    [ DateRangePicker.Today
                    , DateRangePicker.Yesterday
                    , DateRangePicker.PastWeek
                    , DateRangePicker.PastMonth
                    , DateRangePicker.PastYear
                    ]
                , calendarType = calendarDisplay
                , isOpen = False
                , languageConfig = englishLanguageConfig
                }
      , today = Nothing
      , zone = Nothing
      , colorTheme = Light
      }
    , Cmd.batch
        [ Task.perform NewTime Time.now
        , Task.perform NewZone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCalendarDisplay calendarType ->
            let
                newDateRangeSelector =
                    model.datePicker
                        |> DateRangePicker.setCalendarType calendarType
            in
            ( { model
                | calendarDisplay = calendarType
                , datePicker = newDateRangeSelector
              }
            , Cmd.none
            )

        NewTime posix ->
            ( { model | today = Just posix }, Cmd.none )

        DatePickerMsgs msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePicker
            in
            ( { model | datePicker = newDateRangePicker }, Cmd.map DatePickerMsgs dateRangePickerCmd )

        NewZone zone ->
            ( { model | zone = Just zone }, Cmd.none )

        ToggleColorTheme ->
            ( { model
                | colorTheme =
                    if model.colorTheme == Light then
                        Dark

                    else
                        Light
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        styleClass =
            case model.colorTheme of
                Light ->
                    "theme-light open-button"

                Dark ->
                    "theme-dark open-button"

        selector =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass ]
                        [ button [ openDateRangePicker ] [ text "Open Me!" ]
                        , DateRangePicker.view t z model.datePicker
                        ]

                _ ->
                    text ""
    in
    div [ class "main" ]
        [ calendarDisplayOptions model
        , button [ class "toggle-theme", Html.Events.onClick ToggleColorTheme ] [ text "Toggle Color Theme" ]
        , Html.map DatePickerMsgs selector
        ]


calendarDisplayOptions : Model -> Html Msg
calendarDisplayOptions model =
    let
        options =
            [ DateRangePicker.FullCalendar
            , DateRangePicker.ThreeMonths
            , DateRangePicker.TwoMonths
            , DateRangePicker.OneMonth
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
                    DateRangePicker.subscriptions model.datePicker t z

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map DatePickerMsgs selectorSubscriptions
        ]


calendarDisplayToDisplayStr : DateRangePicker.CalendarType -> String
calendarDisplayToDisplayStr calendarDisplay =
    case calendarDisplay of
        DateRangePicker.FullCalendar ->
            "FullCalendar"

        DateRangePicker.ThreeMonths ->
            "ThreeMonths"

        DateRangePicker.TwoMonths ->
            "TwoMonths"

        DateRangePicker.OneMonth ->
            "OneMonth"
