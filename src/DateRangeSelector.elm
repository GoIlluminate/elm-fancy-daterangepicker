module DateRangeSelector exposing (CalendarType, Model, Msg, PosixRange, Selection(..), initModel, openModel, update, view)

import Browser.Events
import Date exposing (Date)
import DateFormat
import DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)
import DateRangePicker.Helper exposing (formatMonth, onClickNoDefault)
import Derberos.Date.Calendar exposing (getCurrentMonthDatesFullWeeks, getFirstDayOfMonth, getFirstDayOfYear, getLastDayOfMonth, getLastDayOfYear)
import Derberos.Date.Core exposing (DateRecord, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, addMonths, addYears, nextWeekdayFromTime, prevWeekdayFromTime)
import Html exposing (Attribute, Html, div, input, table, tbody, td, text, thead)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Json.Decode as Json
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Events exposing (Event(..))
import Return2 as R2
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis, utc)


type Msg
    = DoNothing
    | Open
    | PrevCalendarRange PosixRange
    | NextCalendarRange PosixRange
    | SetSelection Selection
    | OnInputFinish
    | OnInputChange String
    | Reset
    | StartSelection Posix
    | EndSelection Posix
    | KeyDown RawKey
    | KeyUp RawKey
    | TerminateBadState
    | CancelShift


type alias DateRange =
    { start : Date, end : Date }


type alias PosixRange =
    { start : Posix, end : Posix }


type PresetType
    = Today
    | Yesterday
    | PastWeek
    | PastMonth
    | PastYear
    | Custom CustomPreset


type Interval
    = Days
    | Months
    | Weeks
    | Years


type alias CustomPreset =
    { interval : Interval, intervalValue : Int, display : String }


type Selection
    = Single Posix
    | Range PosixRange
    | Unselected
    | Selecting PosixRange
    | Preset PresetType


type CalendarType
    = FullCalendar
    | ThreeMonths
    | TwoMonths
    | OneMonth


type alias Model =
    { selection : Selection
    , availableForSelectionStart : Date
    , availableForSelectionEnd : Date
    , visibleCalendarRange : Maybe PosixRange
    , isMouseDown : Bool
    , isShiftDown : Bool
    , presets : List PresetType
    , calendarType : CalendarType
    , isOpen : Bool
    , inputText : String
    }



-- todo add an init with options


initModel : Model
initModel =
    { selection = Unselected
    , availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
    , availableForSelectionEnd = Date.fromCalendarDate 2100 Jan 1
    , visibleCalendarRange = Nothing
    , isMouseDown = False
    , isShiftDown = False
    , presets = []
    , calendarType = FullCalendar
    , isOpen = False
    , inputText = ""
    }


openModel : Attribute Msg
openModel =
    Html.Events.onClick Open


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            R2.withNoCmd model

        Open ->
            R2.withNoCmd { model | isOpen = True }

        PrevCalendarRange currentVisibleRange ->
            updateCalendarRange model -1 currentVisibleRange

        NextCalendarRange currentVisibleRange ->
            updateCalendarRange model 1 currentVisibleRange

        SetSelection selection ->
            R2.withNoCmd { model | selection = selection, inputText = prettyFormatSelection selection }

        OnInputFinish ->
            let
                updatedModel =
                    case parseDateTime model.inputText of
                        Ok value ->
                            -- todo how to do pretty format with time
                            let
                                selection =
                                    convertInput value
                            in
                            { model
                                | selection = selection
                                , inputText = prettyFormatSelection selection
                            }

                        Err _ ->
                            { model | inputText = prettyFormatSelection model.selection }
            in
            R2.withNoCmd updatedModel

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = newText }

        Reset ->
            R2.withNoCmd { model | inputText = "", selection = Unselected }

        StartSelection posix ->
            R2.withNoCmd
                { model
                    | isMouseDown = True
                    , selection = Selecting { start = posix, end = posix }
                }

        EndSelection posix ->
            R2.withNoCmd
                { model
                    | isMouseDown = False
                    , selection = Selecting { start = posix, end = posix }
                }


updateCalendarRange : Model -> Int -> PosixRange -> ( Model, Cmd Msg )
updateCalendarRange model intervalChange currentVisibleRange =
    let
        updateWithIntervalFunc intervalFunc range =
            R2.withNoCmd
                { model
                    | visibleCalendarRange =
                        Just <|
                            { start = intervalFunc intervalChange utc range.start
                            , end = intervalFunc intervalChange utc range.end
                            }
                }

        visibleRange =
            Maybe.withDefault currentVisibleRange model.visibleCalendarRange
    in
    case model.calendarType of
        FullCalendar ->
            updateWithIntervalFunc (\int _ time -> addYears int time) visibleRange

        ThreeMonths ->
            updateWithIntervalFunc addMonths visibleRange

        TwoMonths ->
            updateWithIntervalFunc addMonths visibleRange

        OneMonth ->
            updateWithIntervalFunc addMonths visibleRange


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        shiftSubs =
            if model.isShiftDown then
                [ Keyboard.ups KeyUp
                , Browser.Events.onVisibilityChange CancelShift
                , Keyboard.downs KeyDown
                , Time.every 100 (always TerminateBadState)
                ]

            else
                [ Keyboard.downs KeyDown ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.hoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.hoveredDate |> always)
                ]

            else
                []
    in
    if model.open then
        List.concat [ shiftSubs, mouseSubs ] |> Sub.batch

    else
        Sub.none


view : Posix -> Zone -> Model -> Html Msg
view today zone model =
    let
        -- todo check out the zones used for this, in the morning the zone screws up the date being set in the input box
        visibleRange =
            calcRange today zone model
    in
    div [ Attrs.class "elm-fancy--daterangepicker" ]
        [ topBar model
        , rangeSelector visibleRange
        , calendarView today zone model visibleRange
        ]


topBar : Model -> Html Msg
topBar model =
    div [ Attrs.class "top-bar" ]
        [ div [ Attrs.class "top-bar--reset", Html.Events.onClick Reset ]
            [ text "Reset" ]
        , calendarInput model
        ]


calendarInput : Model -> Html Msg
calendarInput model =
    div [ Attrs.class "calendar-input" ]
        [ input
            [ Keyboard.Events.on Keypress [ ( Enter, OnInputFinish ) ]
            , Html.Events.onBlur OnInputFinish
            , Html.Events.onInput OnInputChange
            , Attrs.placeholder "Start date - End date"
            , Attrs.value model.inputText
            ]
            []
        ]


convertInput : Input -> Selection
convertInput input =
    case input of
        SingleInput inputDate ->
            convertInputDate inputDate

        RangeInput start end ->
            combineInputToRange start end


combineInputToRange : InputDate -> InputDate -> Selection
combineInputToRange start end =
    let
        startSelection =
            convertInputDate start

        endSelection =
            convertInputDate end
    in
    case ( startSelection, endSelection ) of
        ( Single startPosix, Single endPosix ) ->
            Range { start = startPosix, end = endPosix }

        ( Single startPosix, Range endPosixRange ) ->
            Range { start = startPosix, end = endPosixRange.end }

        ( Range startPosixRange, Single endPosix ) ->
            Range { start = startPosixRange.start, end = endPosix }

        ( Range startPosixRange, Range endPosixRange ) ->
            Range { start = startPosixRange.start, end = endPosixRange.end }

        _ ->
            Unselected


convertInputDate : InputDate -> Selection
convertInputDate inputDate =
    case inputDate of
        JustYear year ->
            Range <| yearToPosixRange year

        JustYearAndMonth yearAndMonth ->
            Range <| yearAndMonthToPosixRange yearAndMonth

        FullDate dateParts ->
            Range <| datePartsToPosixRange dateParts

        FullDateTime dateTimeParts ->
            Single <| dateTimePartsToPosix dateTimeParts utc


rangeSelector : PosixRange -> Html Msg
rangeSelector visibleRange =
    -- todo make last things be end of day instead of beginning
    let
        selection =
            Range { start = visibleRange.start, end = visibleRange.end }
    in
    div
        [ Attrs.class "range-selector" ]
        [ div [ Attrs.class "range-selector--button", onClick <| PrevCalendarRange visibleRange ] [ text "❮" ]
        , div [ Attrs.class "range-selector--year", onClick <| SetSelection selection ]
            [ text <| String.fromInt <| Time.toYear utc visibleRange.start ]
        , div [ Attrs.class "range-selector--button", onClick <| NextCalendarRange visibleRange ] [ text "❯" ]
        ]


calendarView : Posix -> Zone -> Model -> PosixRange -> Html Msg
calendarView today zone model visibleRange =
    case model.calendarType of
        FullCalendar ->
            yearCalendarView today zone model visibleRange

        ThreeMonths ->
            div [] []

        TwoMonths ->
            div [] []

        OneMonth ->
            div [] []


yearCalendarView : Posix -> Zone -> Model -> PosixRange -> Html Msg
yearCalendarView today zone model visibleRange =
    let
        posixMonths =
            List.map
                (\x ->
                    addMonths x utc <| getFirstDayOfYear zone visibleRange.start
                )
            <|
                List.range 0 11

        quarter name =
            div [] [ text name ]

        quarters =
            div
                [ Attrs.class "quarters" ]
                [ quarter "Q1", quarter "Q2", quarter "Q3", quarter "Q4" ]

        presets =
            div [ Attrs.class "presets" ] []
    in
    div [ Attrs.class "second-row" ]
        [ quarters
        , table [] [ tbody [ Attrs.class "year" ] <| List.map (\m -> monthCalendarView m today zone model) posixMonths ]
        , presets
        ]


monthCalendarView : Posix -> Posix -> Zone -> Model -> Html Msg
monthCalendarView currentMonth today zone model =
    let
        selection =
            Range { start = getFirstDayOfMonth utc currentMonth, end = getLastDayOfMonth utc currentMonth }
    in
    td []
        [ table []
            [ thead [ Attrs.class "month--header", onClick <| SetSelection selection ]
                [ text <| formatMonth <| Time.toMonth utc currentMonth ]
            , tbody [ Attrs.class "month" ] <|
                List.map (\x -> dayCalendarView zone currentMonth x today model) <|
                    getCurrentMonthDatesFullWeeks utc currentMonth
            ]
        ]


dayCalendarView : Zone -> Posix -> Posix -> Posix -> Model -> Html Msg
dayCalendarView zone currentMonth currentDay today model =
    let
        monthOfDate =
            Time.toMonth utc

        wantedMonth =
            monthOfDate currentMonth

        content =
            if monthOfDate currentDay == wantedMonth then
                [ text <| String.fromInt <| Time.toDay utc currentDay ]

            else
                []

        setDate =
            if model.isShiftDown || model.isMouseDown then
                Just currentDay |> EndSelection |> onClickNoDefault

            else
                StartSelection currentDay |> DateRangePicker.Helper.mouseDownNoDefault
    in
    td [ Attrs.class "day" ] content


calcRange : Posix -> Zone -> Model -> PosixRange
calcRange today zone model =
    let
        convertToRange =
            case model.calendarType of
                FullCalendar ->
                    { start = getFirstDayOfYear zone today, end = getLastDayOfYear zone today }

                ThreeMonths ->
                    { start = getFirstDayOfMonth zone <| addMonths -1 utc today
                    , end = getLastDayOfMonth zone <| addMonths 1 utc today
                    }

                TwoMonths ->
                    { start = getFirstDayOfMonth zone <| addMonths -1 utc today, end = getLastDayOfMonth zone today }

                OneMonth ->
                    { start = getFirstDayOfMonth zone today, end = getLastDayOfMonth zone today }
    in
    Maybe.withDefault convertToRange model.visibleCalendarRange


prettyFormatSelection : Selection -> String
prettyFormatSelection selection =
    -- todo handling time zones
    case selection of
        Single posix ->
            singleFormatter utc posix

        Range posixRange ->
            fullFormatter utc posixRange.start posixRange.end

        Unselected ->
            ""

        Selecting _ ->
            ""

        Preset presetType ->
            -- todo create date from this or show string?
            ""


singleFormatter : Zone -> Posix -> String
singleFormatter =
    DateFormat.format
        [ DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]


fullFormatter : Zone -> Posix -> Posix -> String
fullFormatter zone start end =
    singleFormatter zone start ++ " to " ++ singleFormatter zone end



-- Copied from Derberos.Date.Utils and edited to make sunday first day of the week


getCurrentMonthDatesFullWeeks : Zone -> Posix -> List Posix
getCurrentMonthDatesFullWeeks zone time =
    let
        firstDayOfMonth =
            time
                |> getFirstDayOfMonth zone
                |> prevWeekdayFromTime Sun zone

        lastDayOfMonth =
            time
                |> getLastDayOfMonth zone
                |> nextWeekdayFromTime Sat zone

        numberDaysInMonth =
            (posixToMillis lastDayOfMonth - posixToMillis firstDayOfMonth) // (1000 * 60 * 60 * 24)
    in
    List.range 0 numberDaysInMonth
        |> List.map (\delta -> addDays delta firstDayOfMonth)


yearToPosixRange : Int -> PosixRange
yearToPosixRange year =
    let
        posix =
            yearToPosix year utc
    in
    { start = getStartOfDay <| getFirstDayOfYear utc posix
    , end = getEndOfDay <| getLastDayOfYear utc posix
    }


yearAndMonthToPosixRange : YearAndMonth -> PosixRange
yearAndMonthToPosixRange yearMonth =
    let
        posix =
            yearAndMonthToPosix yearMonth utc
    in
    { start = getStartOfDay <| getFirstDayOfMonth utc posix
    , end = getEndOfDay <| getLastDayOfMonth utc posix
    }


datePartsToPosixRange : DateParts -> PosixRange
datePartsToPosixRange dateParts =
    let
        posix =
            datePartsToPosix dateParts utc
    in
    { start = getStartOfDay posix, end = getEndOfDay posix }


getEndOfDay : Posix -> Posix
getEndOfDay posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | hour = 23
                , minute = 59
                , second = 0
                , millis = 0
            }
    in
    civilToPosix updatedDateRecord


getStartOfDay : Posix -> Posix
getStartOfDay posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | hour = 0
                , minute = 0
                , second = 0
                , millis = 0
            }
    in
    civilToPosix updatedDateRecord
