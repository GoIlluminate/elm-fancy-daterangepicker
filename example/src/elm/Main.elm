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
    | DatePickerMsgs1 DateRangePicker.Msg
    | DatePickerMsgs2 DateRangePicker.Msg
    | DatePickerMsgs3 DateRangePicker.Msg
    | DatePickerMsgs4 DateRangePicker.Msg
    | ToggleColorTheme


type ColorTheme
    = Light
    | Dark


type alias Model =
    { calendarDisplay : DateRangePicker.CalendarType
    , datePicker1 : DateRangePicker.Model
    , datePicker2 : DateRangePicker.Model
    , datePicker3 : DateRangePicker.Model
    , datePicker4 : DateRangePicker.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    , colorTheme : ColorTheme
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            DateRangePicker.FullCalendar

        initDatePicker buttonId =
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
                , buttonId = buttonId
                }
    in
    ( { calendarDisplay = calendarDisplay
      , datePicker1 = initDatePicker "datepicker--button-1"
      , datePicker2 = initDatePicker "datepicker--button-2"
      , datePicker3 = initDatePicker "datepicker--button-3"
      , datePicker4 = initDatePicker "datepicker--button-4"
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
                , datePicker1 = newDateRangeSelector model.datePicker1
                , datePicker2 = newDateRangeSelector model.datePicker2
                , datePicker3 = newDateRangeSelector model.datePicker3
                , datePicker4 = newDateRangeSelector model.datePicker4
              }
            , Cmd.none
            )

        NewTime posix ->
            ( { model | today = Just posix }, Cmd.none )

        DatePickerMsgs1 msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePicker1
            in
            ( { model | datePicker1 = newDateRangePicker }, Cmd.map DatePickerMsgs1 dateRangePickerCmd )

        DatePickerMsgs2 msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePicker2
            in
            ( { model | datePicker2 = newDateRangePicker }, Cmd.map DatePickerMsgs2 dateRangePickerCmd )

        DatePickerMsgs3 msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePicker3
            in
            ( { model | datePicker3 = newDateRangePicker }, Cmd.map DatePickerMsgs3 dateRangePickerCmd )

        DatePickerMsgs4 msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ model.datePicker4
            in
            ( { model | datePicker4 = newDateRangePicker }, Cmd.map DatePickerMsgs4 dateRangePickerCmd )

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
                        [ button [ open, id buttonId ] [ text "Open Me!" ]
                        , DateRangePicker.view t z datepicker
                        , getStats (getLocal datepicker) (utcSelection datepicker)
                        ]

                _ ->
                    text ""

        selector1 =
            getSelector model.datePicker1 model.datePicker1.buttonId ""

        selector2 =
            getSelector model.datePicker2 model.datePicker2.buttonId ""

        selector3 =
            getSelector model.datePicker3 model.datePicker3.buttonId "bottom-left"

        selector4 =
            getSelector model.datePicker4 model.datePicker4.buttonId "bottom-right"

        getLocal datepicker =
            case model.today of
                Just t ->
                    case ( DateRangePicker.getLocalSelectionRange t datepicker, model.zone ) of
                        ( Just pos, Just tz ) ->
                            ( DateRangePicker.fullFormatter datepicker.languageConfig DateRangePicker.DateTimeFormat Time.utc pos.start pos.end
                            , DateCore.getTzOffset tz pos.start
                            )

                        _ ->
                            ( "", 0 )

                Nothing ->
                    ( "", 0 )

        utcSelection datepicker =
            case model.today of
                Just t ->
                    case DateRangePicker.getUtcSelectionRange (Maybe.withDefault Time.utc model.zone) t datepicker of
                        Just range ->
                            DateRangePicker.fullFormatter datepicker.languageConfig DateRangePicker.DateTimeFormat Time.utc range.start range.end

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
        [ Html.map DatePickerMsgs1 selector1
        , calendarDisplayOptions model
        , Html.map DatePickerMsgs3 selector3
        , Html.map DatePickerMsgs2 selector2
        , div [] []
        , button [ class "toggle-theme", Html.Events.onClick ToggleColorTheme ] [ text "Toggle Color Theme" ]
        , Html.map DatePickerMsgs4 selector4
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
        [ Sub.map DatePickerMsgs1 (selectorSubscriptions model.datePicker1)
        , Sub.map DatePickerMsgs2 (selectorSubscriptions model.datePicker2)
        , Sub.map DatePickerMsgs3 (selectorSubscriptions model.datePicker3)
        , Sub.map DatePickerMsgs4 (selectorSubscriptions model.datePicker4)
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
