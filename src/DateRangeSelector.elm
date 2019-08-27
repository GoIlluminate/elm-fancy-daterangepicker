module DateRangeSelector exposing (CalendarType(..), Config, CustomPreset, Interval(..), LanguageConfig, Model, Msg, PosixRange, PresetType(..), Selection(..), englishLanugageConfig, initModel, initModelWithOptions, openDateRangePicker, presetToDisplayString, presetToPosixRange, subscriptions, update, view)

import Browser.Dom exposing (Element, Error, getElement)
import Browser.Events
import Date exposing (Date)
import DateFormat
import DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)
import DateRangePicker.Helper exposing (formatMonth, onClickNoDefault)
import Derberos.Date.Calendar exposing (getCurrentMonthDatesFullWeeks, getFirstDayOfMonth, getFirstDayOfYear, getLastDayOfMonth, getLastDayOfYear)
import Derberos.Date.Core exposing (DateRecord, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, addMonths, addYears, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (monthToNumber1)
import Html exposing (Attribute, Html, button, div, input, table, tbody, td, text, thead)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Events exposing (Event(..))
import Return2 as R2
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis, utc)


type Msg
    = DoNothing
    | Open
    | Close
    | PrevCalendarRange PosixRange
    | NextCalendarRange PosixRange
    | SetSelection Selection
    | OnInputFinish Zone
    | OnInputChange String
    | Reset
    | StartSelection Posix
    | EndSelection (Maybe Posix)
    | KeyDown RawKey
    | KeyUp RawKey
    | TerminateBadState
    | CancelShift
    | OnHoverOverDay Posix
    | OnMouseMove Mouse.Event
    | SetMouseOutside Bool
    | OnGetElementSuccess (Result Error Element)
    | CheckToMoveToNextVisibleRange Posix Zone
    | SetPresetMenu Bool
    | SelectPreset PresetType


type MousePosition
    = Inside
    | OutsideTop
    | OutsideLeft
    | OutsideRight
    | OutsideBottom


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
    { intervalStart : Interval
    , intervalStartValue : Int
    , intervalEnd : Interval
    , intervalEndValue : Int
    , display : String
    }


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
    , terminationCounter : Int
    , currentlyHoveredDate : Maybe Posix
    , mousePosition : Maybe Mouse.Event
    , uiElement : Maybe Element
    , isMouseOutside : Bool
    , languageConfig : LanguageConfig
    , isPresetMenuOpen : Bool
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
    , terminationCounter = 0
    , currentlyHoveredDate = Nothing
    , mousePosition = Nothing
    , uiElement = Nothing
    , isMouseOutside = False
    , languageConfig = englishLanugageConfig
    , isPresetMenuOpen = False
    }


type alias Config =
    { availableForSelectionStart : Date
    , availableForSelectionEnd : Date
    , presets : List PresetType
    , calendarType : CalendarType
    , isOpen : Bool
    , languageConfig : LanguageConfig
    }


type alias LanguageConfig =
    { fullCalendarSelection : String
    , done : String
    , reset : String
    , inputPlaceholder : String
    , presets : String
    }


englishLanugageConfig : LanguageConfig
englishLanugageConfig =
    { fullCalendarSelection = "Select all of"
    , done = "Done"
    , reset = "Reset"
    , inputPlaceholder = "Start date - End date"
    , presets = "Presets"
    }


initModelWithOptions : Config -> Model
initModelWithOptions config =
    { initModel
        | availableForSelectionStart = config.availableForSelectionStart
        , availableForSelectionEnd = config.availableForSelectionEnd
        , presets = config.presets
        , calendarType = config.calendarType
        , isOpen = config.isOpen
    }


openDateRangePicker : Attribute Msg
openDateRangePicker =
    Html.Events.onClick Open


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            R2.withNoCmd model

        Open ->
            R2.withCmds
                [ Task.attempt OnGetElementSuccess <|
                    getElement "elm-fancy--daterangepicker-calendar"
                ]
                { model | isOpen = True }

        Close ->
            R2.withNoCmd { model | isOpen = False }

        PrevCalendarRange currentVisibleRange ->
            updateCalendarRange model -1 currentVisibleRange

        NextCalendarRange currentVisibleRange ->
            updateCalendarRange model 1 currentVisibleRange

        SetSelection selection ->
            R2.withNoCmd { model | selection = selection, inputText = prettyFormatSelection selection }

        OnInputFinish zone ->
            let
                updatedModel =
                    case parseDateTime model.inputText of
                        Ok value ->
                            -- todo how to do pretty format with time
                            let
                                selection =
                                    convertInput value zone
                            in
                            { model
                                | selection = selection
                                , inputText = prettyFormatSelection selection
                                , visibleCalendarRange = getVisibleRangeFromSelection selection
                            }

                        Err _ ->
                            { model | inputText = prettyFormatSelection model.selection }
            in
            R2.withNoCmd updatedModel

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = newText }

        Reset ->
            R2.withNoCmd { model | inputText = "", selection = Unselected, visibleCalendarRange = Nothing }

        StartSelection posix ->
            R2.withNoCmd
                { model
                    | isMouseDown = True
                    , selection = Selecting { start = posix, end = posix }
                }

        EndSelection posix ->
            case posix of
                Just p ->
                    let
                        selection =
                            Range <| normalizeSelectingRange <| createSelectingRange model p
                    in
                    R2.withNoCmd
                        { model
                            | isMouseDown = False
                            , selection = selection
                            , inputText = prettyFormatSelection selection
                        }

                Nothing ->
                    R2.withNoCmd model

        KeyDown rawKey ->
            onShiftKey rawKey
                model
                (\m ->
                    if m.isShiftDown then
                        R2.withNoCmd
                            { m
                                | terminationCounter =
                                    if m.terminationCounter >= 2 then
                                        m.terminationCounter

                                    else
                                        m.terminationCounter + 1
                            }

                    else
                        -- add start selection on shift logic
                        R2.withNoCmd { m | isShiftDown = True }
                )

        KeyUp rawKey ->
            onShiftKey rawKey model cancelShift

        TerminateBadState ->
            -- todo what should dates be set to on terminate
            if model.terminationCounter < 0 then
                R2.withNoCmd
                    { model
                        | isShiftDown = False
                        , isMouseDown = False

                        --                    , endDate = Maybe.map .end model.dateRange
                        --                    , startDate = Maybe.map .start model.dateRange
                        , terminationCounter = 10
                    }

            else
                R2.withNoCmd { model | terminationCounter = model.terminationCounter - 1 }

        CancelShift ->
            cancelShift model

        OnHoverOverDay posix ->
            let
                selection =
                    if model.isMouseDown || model.isShiftDown then
                        Selecting <| createSelectingRange model posix

                    else
                        model.selection
            in
            R2.withNoCmd { model | currentlyHoveredDate = Just posix, selection = selection }

        OnMouseMove event ->
            R2.withNoCmd { model | mousePosition = Just event }

        SetMouseOutside bool ->
            R2.withNoCmd { model | isMouseOutside = bool }

        OnGetElementSuccess result ->
            case result of
                Ok element ->
                    R2.withNoCmd { model | uiElement = Just element }

                Err _ ->
                    R2.withNoCmd model

        CheckToMoveToNextVisibleRange today zone ->
            let
                visibleRange =
                    calcRange today zone model
            in
            case ( model.uiElement, model.mousePosition, model.isMouseOutside ) of
                ( Just element, Just position, True ) ->
                    case calculateMousePosition element position of
                        OutsideRight ->
                            updateCalendarRange model 1 visibleRange

                        OutsideLeft ->
                            updateCalendarRange model -1 visibleRange

                        _ ->
                            R2.withNoCmd model

                _ ->
                    R2.withNoCmd model

        SetPresetMenu bool ->
            R2.withNoCmd { model | isPresetMenuOpen = bool }

        SelectPreset presetType ->
            let
                selection =
                    Preset presetType
            in
            R2.withNoCmd
                { model
                    | isPresetMenuOpen = False
                    , selection = selection
                    , inputText = prettyFormatSelection selection
                }


calculateMousePosition : Element -> Mouse.Event -> MousePosition
calculateMousePosition element event =
    let
        ( mouseX, mouseY ) =
            event.clientPos
    in
    if mouseX > (element.element.x + element.element.width) then
        OutsideRight

    else if mouseX < element.element.x then
        OutsideLeft

    else if mouseY < element.element.y then
        OutsideTop

    else if mouseY > (element.element.y + element.element.height) then
        OutsideBottom

    else
        Inside


createSelectingRange : Model -> Posix -> PosixRange
createSelectingRange model changedValue =
    case model.selection of
        Single posix ->
            { start = changedValue, end = changedValue }

        Range posixRange ->
            { start = posixRange.start, end = changedValue }

        Unselected ->
            { start = changedValue, end = changedValue }

        Selecting posixRange ->
            { start = posixRange.start, end = changedValue }

        Preset presetType ->
            { start = changedValue, end = changedValue }


cancelShift : Model -> ( Model, Cmd Msg )
cancelShift model =
    case model.selection of
        Selecting posixRange ->
            let
                selection =
                    PosixRange posixRange.start posixRange.end
                        |> normalizeSelectingRange
                        |> Range
            in
            R2.withNoCmd
                { model
                    | isShiftDown = False
                    , terminationCounter = 10
                    , selection = selection
                    , inputText = prettyFormatSelection selection
                }

        _ ->
            R2.withNoCmd
                { model
                    | isShiftDown = False
                    , terminationCounter = 10
                }


onShiftKey : RawKey -> Model -> (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
onShiftKey rawKey model onShiftFunc =
    case Keyboard.anyKeyOriginal rawKey of
        Just key ->
            case key of
                Shift ->
                    onShiftFunc model

                _ ->
                    R2.withNoCmd model

        Nothing ->
            R2.withNoCmd model


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


subscriptions : Model -> Posix -> Zone -> Sub Msg
subscriptions model today zone =
    let
        shiftSubs =
            if model.isShiftDown then
                [ Keyboard.ups KeyUp
                , Browser.Events.onVisibilityChange (always CancelShift)
                , Time.every 100 (always TerminateBadState)
                ]

            else
                []

        keyDowns =
            [ Keyboard.downs KeyDown ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.currentlyHoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.currentlyHoveredDate |> always)
                , Time.every 1250 (always <| CheckToMoveToNextVisibleRange today zone)
                ]

            else
                []
    in
    if model.isOpen then
        List.concat [ shiftSubs, mouseSubs, keyDowns ] |> Sub.batch

    else
        Sub.none


view : Posix -> Zone -> Model -> Html Msg
view today zone model =
    let
        visibleRange =
            calcRange today zone model

        mouseEvent =
            if model.isMouseDown && model.isMouseOutside then
                Mouse.onMove OnMouseMove

            else
                Attrs.class ""
    in
    if model.isOpen then
        div [ Attrs.class "elm-fancy--daterangepicker" ]
            [ div
                [ Attrs.class "close"
                , Html.Events.onClick Close
                , mouseEvent
                , Html.Events.onMouseLeave <| SetMouseOutside False
                , Html.Events.onMouseEnter <| SetMouseOutside True
                ]
                []
            , div [ Attrs.class "body" ]
                [ topBar model zone visibleRange
                , leftSelector visibleRange
                , rightSelector visibleRange
                , calendarView today zone model visibleRange
                , bottomBar model
                ]
            ]

    else
        text ""


presets : Model -> Html Msg
presets model =
    if List.isEmpty model.presets then
        div [] []

    else if model.isPresetMenuOpen then
        presetMenu model

    else
        div [ Html.Events.onClick <| SetPresetMenu True ] [ text "Presets" ]


presetMenu : Model -> Html Msg
presetMenu model =
    div [ Attrs.class "preset-menu--container" ]
        [ div [ Attrs.class "preset-menu--close", Html.Events.onClick <| SetPresetMenu False ]
            []
        , div [ Attrs.class "preset-menu--content" ] <|
            List.map
                (\p ->
                    div [ Html.Events.onClick <| SelectPreset p, Attrs.class "menu-item" ]
                        [ text <| presetToDisplayString p ]
                )
                model.presets
        ]


bottomBar : Model -> Html Msg
bottomBar model =
    div [ Attrs.class "bottom-bar" ]
        [ button [ Attrs.class "done", Html.Events.onClick Close ] [ text model.languageConfig.done ]
        , div [ Attrs.class "reset", Html.Events.onClick Reset ]
            [ text model.languageConfig.reset ]
        ]


leftSelector : PosixRange -> Html Msg
leftSelector visibleRange =
    div
        [ Attrs.class "prev-range-selector", onClick <| PrevCalendarRange visibleRange ]
        [ div [] [ text "❮" ] ]


rightSelector : PosixRange -> Html Msg
rightSelector visibleRange =
    div [ Attrs.class "next-range-selector", onClick <| NextCalendarRange visibleRange ]
        [ div [] [ text "❯" ] ]


topBar : Model -> Zone -> PosixRange -> Html Msg
topBar model zone visibleRange =
    let
        fullCalendarSelector =
            case model.calendarType of
                FullCalendar ->
                    div [ Attrs.class "full-calendar-selector", onClick <| SetSelection selection ]
                        [ text <| selectionText model visibleRange ]

                _ ->
                    text ""

        selection =
            Range { start = visibleRange.start, end = visibleRange.end }
    in
    div [ Attrs.class "top-bar" ]
        [ fullCalendarSelector
        , presets model
        , calendarInput model zone
        ]


selectionText : Model -> PosixRange -> String
selectionText model visibleRange =
    model.languageConfig.fullCalendarSelection ++ " " ++ (String.fromInt <| Time.toYear utc visibleRange.start)


calendarInput : Model -> Zone -> Html Msg
calendarInput model zone =
    div [ Attrs.class "calendar-input" ]
        [ input
            [ Keyboard.Events.on Keypress [ ( Enter, OnInputFinish zone ) ]
            , Html.Events.onBlur <| OnInputFinish zone
            , Html.Events.onInput OnInputChange
            , Attrs.placeholder model.languageConfig.inputPlaceholder
            , Attrs.value model.inputText
            ]
            []
        ]


convertInput : Input -> Zone -> Selection
convertInput input zone =
    case input of
        SingleInput inputDate ->
            convertInputDate inputDate zone

        RangeInput start end ->
            combineInputToRange start end zone


combineInputToRange : InputDate -> InputDate -> Zone -> Selection
combineInputToRange start end zone =
    let
        startSelection =
            convertInputDate start zone

        endSelection =
            convertInputDate end zone
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


convertInputDate : InputDate -> Zone -> Selection
convertInputDate inputDate zone =
    case inputDate of
        JustYear year ->
            Range <| yearToPosixRange year zone

        JustYearAndMonth yearAndMonth ->
            Range <| yearAndMonthToPosixRange yearAndMonth zone

        FullDate dateParts ->
            Range <| datePartsToPosixRange dateParts zone

        FullDateTime dateTimeParts ->
            Single <| dateTimePartsToPosix dateTimeParts utc


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

        quarter name startMonth endMonth =
            div
                [ posixRangeForMonths startMonth endMonth (Time.toYear zone visibleRange.start) zone
                    |> Range
                    |> SetSelection
                    |> Html.Events.onClick
                , Attrs.class "selection-hover"
                ]
                [ text name ]

        quarters =
            div
                [ Attrs.class "quarters" ]
                [ quarter "Q1" Jan Mar, quarter "Q2" Apr Jun, quarter "Q3" Jul Sep, quarter "Q4" Oct Dec ]
    in
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "year-calendar" ]
        [ quarters
        , table [] [ tbody [ Attrs.class "year" ] <| List.map (\m -> monthCalendarView m today zone model) posixMonths ]
        ]


posixRangeForMonths : Month -> Month -> Int -> Zone -> PosixRange
posixRangeForMonths startMonth endMonth currentYear zone =
    let
        start =
            yearAndMonthToPosixRange { year = currentYear, month = monthToNumber1 startMonth } zone

        end =
            yearAndMonthToPosixRange { year = currentYear, month = monthToNumber1 endMonth } zone
    in
    { start = start.start, end = end.end }


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
        -- todo prevent all interaction with invisible days (from other months)
        monthOfDate =
            Time.toMonth utc

        wantedMonth =
            monthOfDate currentMonth

        contentIsInCorrectMonth =
            monthOfDate currentDay == wantedMonth

        content =
            if contentIsInCorrectMonth then
                [ text <| String.fromInt <| Time.toDay utc currentDay ]

            else
                []

        setDate =
            if model.isShiftDown || model.isMouseDown then
                Just currentDay |> EndSelection |> onClickNoDefault

            else
                StartSelection currentDay |> DateRangePicker.Helper.mouseDownNoDefault

        isSameDayOfSelection getPosixFromSelection =
            contentIsInCorrectMonth && (Maybe.withDefault False <| Maybe.map (\p -> isSameDay p currentDay) (getPosixFromSelection model.selection))

        classList =
            Attrs.classList
                [ ( "day", True )
                , ( "selected-range", contentIsInCorrectMonth && isInSelectionRange currentDay model today zone )
                , ( "border-selection", isSameDayOfSelection selectionStart || isSameDayOfSelection selectionEnd )

                -- todo check if zone is correct
                , ( "today", isSameDay currentDay today )
                ]
    in
    td [ classList, setDate, Html.Events.onMouseOver <| OnHoverOverDay currentDay ] content


normalizeSelectingRange : PosixRange -> PosixRange
normalizeSelectingRange posixRange =
    if posixToMillis posixRange.start > posixToMillis posixRange.end then
        { start = posixRange.end, end = posixRange.start }

    else
        posixRange


isInSelectionRange : Posix -> Model -> Posix -> Zone -> Bool
isInSelectionRange comparisonPosix model today localZone =
    let
        posixInMillis =
            posixToMillis comparisonPosix

        compareRange range =
            posixToMillis range.start <= posixInMillis && posixInMillis <= posixToMillis range.end
    in
    case model.selection of
        Single posix ->
            -- todo is this concept getting removed?
            False

        Range posixRange ->
            compareRange posixRange

        Unselected ->
            False

        Selecting posixRange ->
            compareRange <| normalizeSelectingRange posixRange

        Preset presetType ->
            compareRange <| presetToPosixRange presetType today localZone


selectionEnd : Selection -> Maybe Posix
selectionEnd selection =
    -- todo try to combine all these things that are casing
    case selection of
        Single posix ->
            Nothing

        Range posixRange ->
            Just posixRange.end

        Unselected ->
            Nothing

        Selecting posixRange ->
            Just <| .end <| normalizeSelectingRange posixRange

        Preset presetType ->
            --todo on this
            Nothing


selectionStart : Selection -> Maybe Posix
selectionStart selection =
    case selection of
        Single posix ->
            Nothing

        Range posixRange ->
            Just posixRange.start

        Unselected ->
            Nothing

        Selecting posixRange ->
            Just <| .start <| normalizeSelectingRange posixRange

        Preset presetType ->
            --todo on this
            Nothing


isSameDay : Posix -> Posix -> Bool
isSameDay posix1 posix2 =
    let
        civel1 =
            posixToCivil posix1

        civel2 =
            posixToCivil posix2
    in
    civel1.day == civel2.day && civel1.month == civel2.month && civel1.year == civel2.year


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


getVisibleRangeFromSelection : Selection -> Maybe PosixRange
getVisibleRangeFromSelection selection =
    case selection of
        Single posix ->
            Just { start = getStartOfDay posix, end = getEndOfDay posix }

        Range posixRange ->
            Just posixRange

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        Preset presetType ->
            Nothing



-- todo update when preset done


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
            presetToDisplayString presetType


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


yearToPosixRange : Int -> Zone -> PosixRange
yearToPosixRange year zone =
    let
        posix =
            yearToPosix year zone
    in
    { start = getStartOfDay <| getFirstDayOfYear utc posix --todo maybe change to zone?
    , end = getEndOfDay <| getLastDayOfYear utc posix
    }


yearAndMonthToPosixRange : YearAndMonth -> Zone -> PosixRange
yearAndMonthToPosixRange yearMonth zone =
    let
        posix =
            yearAndMonthToPosix yearMonth zone
    in
    { start = getStartOfDay <| getFirstDayOfMonth utc posix
    , end = getEndOfDay <| getLastDayOfMonth utc posix
    }


datePartsToPosixRange : DateParts -> Zone -> PosixRange
datePartsToPosixRange dateParts zone =
    let
        posix =
            datePartsToPosix dateParts zone
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
    --todo  maybe call |> adjustMilliseconds zone?
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
    --todo  maybe call |> adjustMilliseconds zone?
    civilToPosix updatedDateRecord



--------------------------------------------------------------------------------------------
--            PRESETS
--------------------------------------------------------------------------------------------
--todo add support for other languages throughout app


presetToDisplayString : PresetType -> String
presetToDisplayString presetType =
    case presetType of
        Today ->
            "Today"

        Yesterday ->
            "Yesterday"

        PastWeek ->
            "Past Week"

        PastMonth ->
            "Past Month"

        PastYear ->
            "Past Year"

        Custom customPreset ->
            customPreset.display


convertInterval : Interval -> Int -> Posix -> Zone -> Posix
convertInterval interval intervalValue today localZone =
    case interval of
        Days ->
            addDays intervalValue today

        Months ->
            addMonths intervalValue localZone today

        Weeks ->
            addDays (intervalValue * -7) today

        Years ->
            addYears intervalValue today


presetToPosixRange : PresetType -> Posix -> Zone -> PosixRange
presetToPosixRange presetType today localZone =
    case presetType of
        Today ->
            { start = getStartOfDay today, end = getEndOfDay today }

        Yesterday ->
            { start = getStartOfDay <| addDays -1 today, end = getEndOfDay <| addDays -1 today }

        PastWeek ->
            { start = getStartOfDay <| addDays -7 today, end = getEndOfDay today }

        PastMonth ->
            -- todo utc?
            { start = getStartOfDay <| addMonths -1 localZone today, end = getEndOfDay today }

        PastYear ->
            { start = getStartOfDay <| addYears -1 today, end = getEndOfDay today }

        Custom customPreset ->
            { start =
                convertInterval customPreset.intervalStart customPreset.intervalStartValue today localZone
                    |> getStartOfDay
            , end =
                convertInterval customPreset.intervalEnd customPreset.intervalEndValue today localZone
                    |> getEndOfDay
            }
