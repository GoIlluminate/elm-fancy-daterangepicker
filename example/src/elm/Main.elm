module Main exposing (Model, Msg(..), calendarDisplayOptions, calendarDisplayToDisplayStr, init, main, subscriptions, update, view)

import Browser
import DateRangePicker exposing (defaultConfig)
import Derberos.Date.Core as DateCore
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra exposing (partsToPosix)


type Msg
    = ChangeCalendarDisplay DateRangePicker.CalendarType
    | NewTime Posix
    | NewZone Zone
    | DatePickerMsgs DateRangePicker.Msg
    | DatePickerMsgsAbs DateRangePicker.Msg
    | DatePickerMsgsFixed DateRangePicker.Msg
    | ToggleColorTheme
    | ChangeToBefore
    | ChangeToAfter


type ColorTheme
    = Light
    | Dark


type alias Model =
    { calendarDisplay : DateRangePicker.CalendarType
    , datePicker : DateRangePicker.DatePicker
    , datePickerabs : DateRangePicker.DatePicker
    , datePickerfixed : DateRangePicker.DatePicker
    , colorTheme : ColorTheme
    }


type BootstrapModel
    = Loading (Maybe Posix) (Maybe Zone)
    | Loaded Model


initialModel : Posix -> Zone -> Model
initialModel now zone =
    let
        calendarDisplay =
            DateRangePicker.FullCalendar

        initDatePicker =
            DateRangePicker.initWithOptions now
                { defaultConfig
                    | presets =
                        [ DateRangePicker.Today
                        , DateRangePicker.Yesterday
                        , DateRangePicker.Custom <|
                            { intervalStart = DateRangePicker.Days
                            , intervalStartValue = -3
                            , intervalEnd = DateRangePicker.Days
                            , intervalEndValue = -1
                            , display = "Past Three Days"
                            }
                        , DateRangePicker.PastWeek
                        , DateRangePicker.PastMonth
                        , DateRangePicker.PastYear
                        ]
                    , calendarType = calendarDisplay
                    , dateSelectionType = DateRangePicker.DateRangeSelection
                    , displayTimezone = zone
                }
    in
    { calendarDisplay = calendarDisplay
    , datePicker = initDatePicker
    , datePickerabs = initDatePicker
    , datePickerfixed = initDatePicker
    , colorTheme = Light
    }


init : ( BootstrapModel, Cmd Msg )
init =
    ( Loading Nothing Nothing
    , Cmd.batch
        [ Task.perform NewTime Time.now
        , Task.perform NewZone Time.here
        ]
    )


update : Msg -> BootstrapModel -> ( BootstrapModel, Cmd Msg )
update msg appModel =
    case appModel of
        Loaded model ->
            case msg of
                ChangeCalendarDisplay calendarType ->
                    let
                        newDateRangeSelector datePicker =
                            datePicker
                                |> DateRangePicker.setCalendarType calendarType
                    in
                    ( Loaded
                        { model
                            | calendarDisplay = calendarType
                            , datePicker = newDateRangeSelector model.datePicker
                        }
                    , Cmd.none
                    )

                DatePickerMsgs msg_ ->
                    let
                        ( newDateRangePicker, dateRangePickerCmd ) =
                            DateRangePicker.update msg_ model.datePicker
                    in
                    ( Loaded { model | datePicker = newDateRangePicker }, Cmd.map DatePickerMsgs dateRangePickerCmd )

                DatePickerMsgsAbs msg_ ->
                    let
                        ( newDateRangePicker, dateRangePickerCmd ) =
                            DateRangePicker.update msg_ model.datePickerabs
                    in
                    ( Loaded { model | datePickerabs = newDateRangePicker }, Cmd.map DatePickerMsgsAbs dateRangePickerCmd )

                DatePickerMsgsFixed msg_ ->
                    let
                        ( newDateRangePicker, dateRangePickerCmd ) =
                            DateRangePicker.update msg_ model.datePickerfixed
                    in
                    ( Loaded { model | datePickerfixed = newDateRangePicker }, Cmd.map DatePickerMsgsFixed dateRangePickerCmd )

                ToggleColorTheme ->
                    ( Loaded
                        { model
                            | colorTheme =
                                if model.colorTheme == Light then
                                    Dark

                                else
                                    Light
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Loaded model, Cmd.none )

        Loading (Just time) (Just zone) ->
            ( Loaded (initialModel time zone), Cmd.none )

        Loading (Just t) Nothing ->
            case msg of
                NewZone z ->
                    ( Loaded (initialModel t z), Cmd.none )

                _ ->
                    ( appModel, Cmd.none )

        Loading Nothing (Just z) ->
            case msg of
                NewTime t ->
                    ( Loaded (initialModel t z), Cmd.none )

                _ ->
                    ( appModel, Cmd.none )

        Loading maybeTime maybeZone ->
            case msg of
                NewTime posix ->
                    ( Loading (Just posix) maybeZone, Cmd.none )

                NewZone z ->
                    ( Loading maybeTime (Just z), Cmd.none )

                _ ->
                    ( appModel, Cmd.none )


view : BootstrapModel -> Html Msg
view bootstrapModel =
    case bootstrapModel of
        Loaded model ->
            let
                styleClass =
                    case model.colorTheme of
                        Light ->
                            "theme-light open-button"

                        Dark ->
                            "theme-dark open-button"

                getSelector datepicker buttonId divClass =
                    div [ class styleClass, class divClass ]
                        [ DateRangePicker.defaultOpener datepicker buttonId
                        , DateRangePicker.view datepicker
                        , getStats (getLocal datepicker) (getSelection datepicker)
                        ]

                selector1 =
                    getSelector model.datePicker "datepicker--button" "relative-class"

                selectorabs =
                    getSelector model.datePickerabs "datepicker--buttonabs" "absolute-class"

                selectorfixed =
                    getSelector model.datePickerfixed "datepicker--buttonfixed" "fixed-class"

                getLocal datepicker =
                    case DateRangePicker.getSelectionRange datepicker of
                        Just pos ->
                            ( DateRangePicker.displaySelection datepicker
                            , DateCore.getTzOffset Time.utc (partsToPosix Time.utc pos.start)
                            )

                        _ ->
                            ( "", 0 )

                getSelection datepicker =
                    DateRangePicker.displaygetSelection datepicker

                getStats ( localSel, _ ) utcSel =
                    div []
                        [ div [] [ text <| "LocalTime: " ++ localSel ]
                        , div [] [ text <| "UtcTime: " ++ utcSel ]
                        ]
            in
            div [ class "main" ]
                [ calendarDisplayOptions model
                , button [ class "toggle-theme", Html.Events.onClick ToggleColorTheme ] [ text "Toggle Color Theme" ]
                , Html.map DatePickerMsgs selector1
                , Html.map DatePickerMsgsAbs selectorabs
                , Html.map DatePickerMsgsFixed selectorfixed
                , div []
                    [ button [ Html.Events.onClick ChangeToBefore ] [ text "before" ]
                    , button [ Html.Events.onClick ChangeToAfter ] [ text "after" ]
                    ]
                ]

        _ ->
            text ""


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


main : Program () BootstrapModel Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : BootstrapModel -> Sub Msg
subscriptions bootstrapModel =
    let
        selectorSubscriptions datepicker =
            DateRangePicker.subscriptions datepicker
    in
    case bootstrapModel of
        Loaded model ->
            Sub.batch
                [ Sub.map DatePickerMsgs (selectorSubscriptions model.datePicker)
                , Sub.map DatePickerMsgsAbs (selectorSubscriptions model.datePickerabs)
                , Sub.map DatePickerMsgsFixed (selectorSubscriptions model.datePickerfixed)
                ]

        _ ->
            Sub.none


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
