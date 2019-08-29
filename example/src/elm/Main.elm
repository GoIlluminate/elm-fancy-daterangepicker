module Main exposing (Model, Msg(..), calendarDisplayOptions, calendarDisplayToDisplayStr, init, main, subscriptions, update, view)

import Browser
import Date
    exposing
        ( Date
        )
import DateRangePicker exposing (englishLanguageConfig, initModelWithOptions, openDateRangePicker)
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
    | DatePickerMsgs2 DateRangePicker.Msg
    | DatePickerMsgs3 DateRangePicker.Msg
    | DatePickerMsgs4 DateRangePicker.Msg
    | ToggleColorTheme


type ColorTheme
    = Light
    | Dark


type alias Model =
    { calendarDisplay : DateRangePicker.CalendarType
    , datePicker : DateRangePicker.Model
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
                , buttonId = "datepicker--button-1"
                }
        , datePicker2 =
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
                , buttonId = "datepicker--button-2"
                }
        , datePicker3 =
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
                , buttonId = "datepicker--button-3"
                }
        , datePicker4 =
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
                , buttonId = "datepicker--button-4"
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
                newDateRangeSelector dp =
                    dp
                        |> DateRangePicker.setCalendarType calendarType
            in
            ( { model
                | calendarDisplay = calendarType
                , datePicker = newDateRangeSelector model.datePicker
                , datePicker2 = newDateRangeSelector model.datePicker2
                , datePicker3 = newDateRangeSelector model.datePicker3
                , datePicker4 = newDateRangeSelector model.datePicker4
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

        selector =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass ]
                        [ button [ openDateRangePicker, id "datepicker--button-1" ] [ text "Open Me!" ]
                        , DateRangePicker.view t z model.datePicker
                        , getStats (getLocal model.datePicker) (utcSelection model.datePicker)
                        ]

                _ ->
                    text ""
        
        selector2 =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass, class "bottom-left" ]
                        [ button [ openDateRangePicker, id "datepicker--button-2" ] [ text "Open Me!" ]
                        , DateRangePicker.view t z model.datePicker2
                        , getStats (getLocal model.datePicker2) (utcSelection model.datePicker2)
                        ]

                _ ->
                    text ""
        
        selector3 =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass ]
                        [ button [ openDateRangePicker, id "datepicker--button-3" ] [ text "Open Me!" ]
                        , DateRangePicker.view t z model.datePicker3
                        , getStats (getLocal model.datePicker3) (utcSelection model.datePicker3)
                        ]

                _ ->
                    text ""
        
        selector4 =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class styleClass, class "bottom-right" ]
                        [ button [ openDateRangePicker, id "datepicker--button-4" ] [ text "Open Me!" ]
                        , DateRangePicker.view t z model.datePicker4
                        , getStats (getLocal model.datePicker4) (utcSelection model.datePicker4)
                        ]

                _ ->
                    text ""

        getLocal dp = 
            case model.today of
                Just t ->
                    case ( DateRangePicker.getLocalSelectionRange t dp, model.zone ) of
                        ( Just pos, Just tz ) ->
                            ( DateRangePicker.fullFormatter dp.languageConfig DateRangePicker.DateTimeFormat Time.utc pos.start pos.end
                            , DateCore.getTzOffset tz pos.start
                            )

                        _ ->
                            ( "", 0 )

                Nothing ->
                    ( "", 0 )

        utcSelection dp =
            case model.today of
                Just t ->
                    case DateRangePicker.getUtcSelectionRange (Maybe.withDefault Time.utc model.zone) t dp of
                        Just range ->
                            DateRangePicker.fullFormatter dp.languageConfig DateRangePicker.DateTimeFormat Time.utc range.start range.end

                        _ ->
                            ""

                Nothing ->
                    ""
        getStats (localSel, localTimezone) utcSel =
            div []
                [ div [] [ text <| "LocalTime: " ++ localSel ]
                , div [] [ text <| "TimeZone: " ++ String.fromInt localTimezone ]
                , div [] [ text <| "UtcTime: " ++ utcSel ]
                ]

    in
    div [ class "main" ]
        [ Html.map DatePickerMsgs selector
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
        selectorSubscriptions dp =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    DateRangePicker.subscriptions dp t z

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map DatePickerMsgs (selectorSubscriptions model.datePicker)
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
