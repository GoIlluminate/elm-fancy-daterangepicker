module Main exposing (Model, Msg(..), calendarDisplayOptions, calendarDisplayToDisplayStr, init, main, subscriptions, update, view)

import Browser
import DateRangePicker exposing (defaultConfig, open)
import Derberos.Date.Core as DateCore
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events
import Task
import Time exposing (Month(..), Posix, Zone)


type Msg
    = ChangeCalendarDisplay DateRangePicker.CalendarType
    | NewTime Posix
    | NewZone Zone
    | DatePickerMsgs DateRangePicker.Msg
    | DatePickerMsgsAbs DateRangePicker.Msg
    | DatePickerMsgsFixed DateRangePicker.Msg
    | ToggleColorTheme


type ColorTheme
    = Light
    | Dark


type alias Model =
    { calendarDisplay : DateRangePicker.CalendarType
    , datePicker : DateRangePicker.Model
    , datePickerabs : DateRangePicker.Model
    , datePickerfixed : DateRangePicker.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    , colorTheme : ColorTheme
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            DateRangePicker.FullCalendar

        initDatePicker =
            DateRangePicker.initWithOptions
                { defaultConfig
                    | presets =
                        [ DateRangePicker.Today
                        , DateRangePicker.Yesterday
                        , DateRangePicker.PastWeek
                        , DateRangePicker.PastMonth
                        , DateRangePicker.PastYear
                        , DateRangePicker.Custom <|
                            { intervalStart = DateRangePicker.Days
                            , intervalStartValue = -3
                            , intervalEnd = DateRangePicker.Days
                            , intervalEndValue = -1
                            , display = "Past Three Days"
                            }
                        ]
                    , calendarType = calendarDisplay
                    , datePickerType = DateRangePicker.DateRangePicker
                }
    in
    ( { calendarDisplay = calendarDisplay
      , datePicker = initDatePicker
      , datePickerabs = initDatePicker
      , datePickerfixed = initDatePicker
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
                newDateRangeSelector datePicker =
                    datePicker
                        |> DateRangePicker.setCalendarType calendarType
            in
            ( { model
                | calendarDisplay = calendarType
                , datePicker = newDateRangeSelector model.datePicker
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

        DatePickerMsgsAbs msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePickerabs
            in
            ( { model | datePickerabs = newDateRangePicker }, Cmd.map DatePickerMsgsAbs dateRangePickerCmd )

        DatePickerMsgsFixed msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePickerfixed
            in
            ( { model | datePickerfixed = newDateRangePicker }, Cmd.map DatePickerMsgsFixed dateRangePickerCmd )

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

        getSelector datepicker buttonId divClass =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass, class divClass ]
                        [ DateRangePicker.defaultOpener model.datePicker buttonId
                        , DateRangePicker.view t z datepicker
                        , getStats (getLocal datepicker) (utcSelection datepicker)
                        ]

                _ ->
                    text ""

        selector1 =
            getSelector model.datePicker "datepicker--button" "relative-class"

        selectorabs =
            getSelector model.datePickerabs "datepicker--buttonabs" "absolute-class"

        selectorfixed =
            getSelector model.datePickerfixed "datepicker--buttonfixed" "fixed-class"

        getLocal datepicker =
            case model.today of
                Just t ->
                    case ( DateRangePicker.localSelectionRange t datepicker, model.zone ) of
                        ( Just pos, Just tz ) ->
                            ( DateRangePicker.fullFormatter datepicker.languageConfig DateRangePicker.DateTimeFormat pos.start pos.end
                            , DateCore.getTzOffset tz pos.start
                            )

                        _ ->
                            ( "", 0 )

                Nothing ->
                    ( "", 0 )

        utcSelection datepicker =
            case model.today of
                Just t ->
                    case DateRangePicker.utcSelectionRange (Maybe.withDefault Time.utc model.zone) t datepicker of
                        Just range ->
                            DateRangePicker.fullFormatter datepicker.languageConfig DateRangePicker.DateTimeFormat range.start range.end

                        _ ->
                            ""

                Nothing ->
                    ""

        getStats ( localSel, localTimezone ) utcSel =
            div []
                [ div [] [ text <| "LocalTime: " ++ localSel ]
                , div [] [ text <| "TimeZone: " ++ String.fromInt localTimezone ]
                , div [] [ text <| "UtcTime: " ++ utcSel ]
                ]
    in
    div [ class "main" ]
        [ calendarDisplayOptions model
        , button [ class "toggle-theme", Html.Events.onClick ToggleColorTheme ] [ text "Toggle Color Theme" ]
        , Html.map DatePickerMsgs selector1
        , Html.map DatePickerMsgsAbs selectorabs
        , Html.map DatePickerMsgsFixed selectorfixed
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
        selectorSubscriptions datepicker =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    DateRangePicker.subscriptions datepicker t z

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map DatePickerMsgs (selectorSubscriptions model.datePicker)
        , Sub.map DatePickerMsgsAbs (selectorSubscriptions model.datePickerabs)
        , Sub.map DatePickerMsgsFixed (selectorSubscriptions model.datePickerfixed)
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
