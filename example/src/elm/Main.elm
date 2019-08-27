module Main exposing (Model, Msg(..), calendarDisplayOptions, calendarDisplayToDisplayStr, init, main, subscriptions, update, view)

import Browser
import Date
    exposing
        ( Date
        )
import DateRangeSelector exposing (englishLanugageConfig, initModelWithOptions, openDateRangePicker)
import Html exposing (Html, button, div, h2, h4, i, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Task
import Time exposing (Month(..), Posix, Zone)


type Msg
    = ChangeCalendarDisplay DateRangeSelector.CalendarType
    | NewTime Posix
    | NewZone Zone
    | NewSelectorMsgs DateRangeSelector.Msg


type alias Model =
    { calendarDisplay : DateRangeSelector.CalendarType
    , dateSelector : DateRangeSelector.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            DateRangeSelector.FullCalendar
    in
    ( { calendarDisplay = calendarDisplay
      , dateSelector =
            initModelWithOptions
                { availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
                , availableForSelectionEnd = Date.fromCalendarDate 2100 Jan 1
                , presets =
                    [ DateRangeSelector.Today
                    , DateRangeSelector.Yesterday
                    , DateRangeSelector.PastWeek
                    , DateRangeSelector.PastMonth
                    , DateRangeSelector.PastYear
                    ]
                , calendarType = calendarDisplay
                , isOpen = False
                , languageConfig = englishLanugageConfig
                }
      , today = Nothing
      , zone = Nothing
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
                    model.dateSelector
                        |> DateRangeSelector.setCalendarType calendarType
            in
            ( { model
                | calendarDisplay = calendarType
                , dateSelector = newDateRangeSelector
              }
            , Cmd.none
            )

        NewTime posix ->
            ( { model | today = Just posix }, Cmd.none )

        NewSelectorMsgs msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangeSelector.update msg_ model.dateSelector
            in
            ( { model | dateSelector = newDateRangePicker }, Cmd.map NewSelectorMsgs dateRangePickerCmd )

        NewZone zone ->
            ( { model | zone = Just zone }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        selector =
            case ( model.today, model.zone ) of
                ( Just t, Just z ) ->
                    div [ class "theme-light open-button" ]
                        [ button [ openDateRangePicker ] [ text "Open Me!" ]
                        , DateRangeSelector.view t z model.dateSelector
                        ]

                _ ->
                    text ""
    in
    div [ class "main" ]
        [ calendarDisplayOptions model
        , Html.map NewSelectorMsgs selector
        ]


calendarDisplayOptions : Model -> Html Msg
calendarDisplayOptions model =
    let
        options =
            [ DateRangeSelector.FullCalendar
            , DateRangeSelector.ThreeMonths
            , DateRangeSelector.TwoMonths
            , DateRangeSelector.OneMonth
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
                    DateRangeSelector.subscriptions model.dateSelector t z

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map NewSelectorMsgs <| selectorSubscriptions
        ]


calendarDisplayToDisplayStr : DateRangeSelector.CalendarType -> String
calendarDisplayToDisplayStr calendarDisplay =
    case calendarDisplay of
        DateRangeSelector.FullCalendar ->
            "FullCalendar"

        DateRangeSelector.ThreeMonths ->
            "ThreeMonths"

        DateRangeSelector.TwoMonths ->
            "TwoMonths"

        DateRangeSelector.OneMonth ->
            "OneMonth"
