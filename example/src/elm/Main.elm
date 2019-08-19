module Main exposing (Model, Msg(..), getSettings, init, main, printDate, printDateRange, update, view)

import Browser
import Date
    exposing
        ( Date
        )
import DateRangePicker exposing (CalendarDisplay(..), DateRange, RestrictedDateRange(..), defaultSettings, getDateRange, setCalendarDisplay, setInputIcon, setSettings)
import DateRangePicker.Helper exposing (formatDate)
import DateRangeSelector exposing (initModel)
import Html exposing (Html, div, h2, h4, i, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Task
import Time exposing (Posix, Zone)


type Msg
    = SetDateRangePicker DateRangePicker.Msg
    | ChangeCalendarDisplay CalendarDisplay
    | NewTime Posix
    | NewZone Zone
    | NewSelectorMsgs DateRangeSelector.Msg


type alias Model =
    { dateRange : Maybe DateRange
    , dateRangePicker : DateRangePicker.DateRangePicker
    , date : Maybe Date
    , calendarDisplay : CalendarDisplay
    , yearsInRange : Maybe Int
    , monthsInRange : Maybe Int
    , weeksInRange : Maybe Int
    , daysInRange : Maybe Int
    , dateSelector : DateRangeSelector.Model
    , today : Maybe Posix
    , zone : Maybe Zone
    }


init : ( Model, Cmd Msg )
init =
    let
        calendarDisplay =
            FullCalendar

        ( dateRangePicker_, dateRangePickerCmd ) =
            DateRangePicker.init

        dateRangePicker =
            dateRangePicker_
                |> setSettings (getSettings True)
                |> DateRangePicker.setInputId "myDateRangePicker"
                |> setInputIcon (i [] [ text "📆" ])
                |> setCalendarDisplay calendarDisplay
    in
    ( { dateRange = Nothing
      , dateRangePicker = dateRangePicker
      , date = Nothing
      , calendarDisplay = calendarDisplay
      , yearsInRange = Nothing
      , monthsInRange = Nothing
      , weeksInRange = Nothing
      , daysInRange = Nothing
      , dateSelector = initModel
      , today = Nothing
      , zone = Nothing
      }
    , Cmd.batch
        [ Cmd.map SetDateRangePicker dateRangePickerCmd
        , Task.perform NewTime Time.now
        , Task.perform NewZone Time.here
        ]
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
update msg ({ dateRangePicker } as model) =
    case msg of
        SetDateRangePicker msg_ ->
            let
                ( newDateRangePicker, dateRangePickerCmd ) =
                    DateRangePicker.update msg_ dateRangePicker

                newDateRange =
                    getDateRange newDateRangePicker

                updatedModel =
                    case newDateRange of
                        Nothing ->
                            { model
                                | dateRangePicker = newDateRangePicker
                                , dateRange = newDateRange
                                , yearsInRange = Nothing
                                , monthsInRange = Nothing
                                , weeksInRange = Nothing
                                , daysInRange = Nothing
                            }

                        Just dr ->
                            { model
                                | dateRangePicker = newDateRangePicker
                                , dateRange = newDateRange
                                , yearsInRange = Just <| DateRangePicker.yearsInRange dr
                                , monthsInRange = Just <| DateRangePicker.monthsInRange dr
                                , weeksInRange = Just <| DateRangePicker.weeksInRange dr
                                , daysInRange = Just <| DateRangePicker.daysInRange dr
                            }
            in
            ( updatedModel
            , Cmd.map SetDateRangePicker dateRangePickerCmd
            )

        ChangeCalendarDisplay calendarDisplay ->
            let
                newDateRangePicker =
                    DateRangePicker.setCalendarDisplay calendarDisplay dateRangePicker
            in
            ( { model
                | dateRangePicker = newDateRangePicker
                , calendarDisplay = calendarDisplay
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
                    div [ class "theme-light" ] [ DateRangeSelector.view t z model.dateSelector ]

                _ ->
                    text ""
    in
    div [ class "main" ]
        [ calendarDisplayOptions model
        , dateRangePickers model
        , Html.map NewSelectorMsgs selector
        ]


calendarDisplayOptions : Model -> Html Msg
calendarDisplayOptions model =
    let
        options =
            [ FullCalendar
            , ThreeMonths
            , TwoMonths
            , OneMonth
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


dateRangePickers : Model -> Html Msg
dateRangePickers model =
    let
        numInRangeView str maybeN =
            case maybeN of
                Nothing ->
                    text ""

                Just n ->
                    div [] [ text <| str ++ " " ++ String.fromInt n ]

        drpView theme =
            div
                [ class "theme--wrapper"
                , class theme
                ]
                [ h2 [] [ text "Date Range Picker" ]
                , h4 [] [ text <| "Selected DateRange: " ++ printDateRange model.dateRange ]
                , DateRangePicker.view model.dateRangePicker |> Html.map SetDateRangePicker
                , div [ class "in-range--container" ]
                    [ numInRangeView "Years in DateRange:" model.yearsInRange
                    , numInRangeView "Months in DateRange:" model.monthsInRange
                    , numInRangeView "Weeks in DateRange:" model.weeksInRange
                    , numInRangeView "Days in DateRange:" model.daysInRange
                    ]
                ]
    in
    div [ class "date-range-picker-wrapper date-picker--wrapper" ]
        [ drpView "theme-light"
        , drpView "theme-dark"
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
        ]


calendarDisplayToDisplayStr : CalendarDisplay -> String
calendarDisplayToDisplayStr calendarDisplay =
    case calendarDisplay of
        FullCalendar ->
            "FullCalendar"

        ThreeMonths ->
            "ThreeMonths"

        TwoMonths ->
            "TwoMonths"

        OneMonth ->
            "OneMonth"
