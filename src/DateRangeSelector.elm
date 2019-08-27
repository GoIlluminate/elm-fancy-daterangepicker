module DateRangeSelector exposing (CalendarType(..), setCalendarType, Config, CustomPreset, Interval(..), LanguageConfig, Model, Msg, PosixRange, PresetType(..), Selection(..), englishLanugageConfig, initModel, initModelWithOptions, openDateRangePicker, presetToDisplayString, presetToPosixRange, subscriptions, update, view)

import Browser.Dom exposing (Element, Error, getElement)
import Browser.Events
import Date exposing (Date)
import DateFormat
import DateFormat.Language as DateFormat
import DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)
import DateRangePicker.Helper exposing (onClickNoDefault)
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
import Svg exposing (g, svg)
import Svg.Attributes as Svg
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis, utc)


type Msg
    = DoNothing
    | Open
    | Close
    | PrevCalendarRange PosixRange
    | NextCalendarRange PosixRange
    | SetSelection Selection
    | OnInputFinish Posix Zone
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
    | SelectPreset PresetType Posix Zone


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
    , calendarType = ThreeMonths
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
    { done : String
    , reset : String
    , inputPlaceholder : String
    , presets : String
    , today : String
    , yesterday : String
    , pastWeek : String
    , pastMonth : String
    , pastYear : String
    , dateFormatLanguage : DateFormat.Language
    }


englishLanugageConfig : LanguageConfig
englishLanugageConfig =
    { done = "Done"
    , reset = "Reset"
    , inputPlaceholder = "Start date - End date"
    , presets = "Presets"
    , today = "Today"
    , yesterday = "Yesterday"
    , pastWeek = "Past Week"
    , pastMonth = "Past Month"
    , pastYear = "Past Year"
    , dateFormatLanguage = DateFormat.english
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
            R2.withNoCmd { model | selection = selection, inputText = prettyFormatSelection selection model.languageConfig }

        OnInputFinish today zone ->
            let
                parseOutput =
                    parseDateTime (List.map (\p -> presetToDisplayString p model.languageConfig) model.presets) model.inputText

                updatedModel =
                    case parseOutput of
                        Ok value ->
                            -- todo how to do pretty format with time
                            let
                                selection =
                                    convertInput value zone model
                            in
                            { model
                                | selection = selection
                                , inputText = prettyFormatSelection selection model.languageConfig
                                , visibleCalendarRange = getVisibleRangeFromSelection selection today zone
                            }

                        Err _ ->
                            { model | inputText = prettyFormatSelection model.selection model.languageConfig }
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
                            , inputText = prettyFormatSelection selection model.languageConfig
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

        SelectPreset presetType today zone ->
            let
                selection =
                    Preset presetType
            in
            R2.withNoCmd
                { model
                    | isPresetMenuOpen = False
                    , selection = selection
                    , visibleCalendarRange = getVisibleRangeFromSelection selection today zone
                    , inputText = prettyFormatSelection selection model.languageConfig
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
                    , inputText = prettyFormatSelection selection model.languageConfig
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
                [ topBar model zone visibleRange today
                , leftSelector visibleRange
                , rightSelector visibleRange
                , calendarView today zone model visibleRange
                , bottomBar model
                ]
            ]

    else
        text ""


presetsDisplay : Model -> Posix -> Zone -> Html Msg
presetsDisplay model today zone =
    if List.isEmpty model.presets then
        div [] []

    else if model.isPresetMenuOpen then
        presetMenu model today zone

    else
        div [ Attrs.class "preset--open--wrapper" ]
            [ div [ Attrs.class "preset--open", Html.Events.onClick <| SetPresetMenu True ]
                [ text <| model.languageConfig.presets, downArrow ]
            ]


presetMenu : Model -> Posix -> Zone -> Html Msg
presetMenu model today zone =
    div [ Attrs.class "preset-menu--container" ]
        [ div [ Attrs.class "preset-menu--close", Html.Events.onClick <| SetPresetMenu False ]
            []
        , div [ Attrs.class "preset-menu--content" ] <|
            List.map
                (\p ->
                    div [ Html.Events.onClick <| SelectPreset p today zone, Attrs.class "menu-item" ]
                        [ text <| presetToDisplayString p model.languageConfig ]
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


topBar : Model -> Zone -> PosixRange -> Posix -> Html Msg
topBar model zone visibleRange today =
    let
        fullCalendarSelector =
            case model.calendarType of
                FullCalendar ->
                    div [ Attrs.class "full-calendar-selector", onClick <| SetSelection selection ]
                        [ text <| selectionText visibleRange ]

                _ ->
                    text ""

        selection =
            Range { start = visibleRange.start, end = visibleRange.end }
    in
    div [ Attrs.class "top-bar" ]
        [ fullCalendarSelector
        , presetsDisplay model today zone
        , calendarInput model zone today
        ]


selectionText : PosixRange -> String
selectionText visibleRange =
    String.fromInt <| Time.toYear utc visibleRange.start


calendarInput : Model -> Zone -> Posix -> Html Msg
calendarInput model zone today =
    div [ Attrs.class "calendar-input" ]
        [ input
            [ Keyboard.Events.on Keypress [ ( Enter, OnInputFinish today zone ) ]
            , Html.Events.onBlur <| OnInputFinish today zone
            , Html.Events.onInput OnInputChange
            , Attrs.placeholder model.languageConfig.inputPlaceholder
            , Attrs.value model.inputText
            ]
            []
        ]


convertInput : Input -> Zone -> Model -> Selection
convertInput input zone model =
    case input of
        SingleInput inputDate ->
            convertInputDate inputDate zone

        RangeInput start end ->
            combineInputToRange start end zone

        CustomDate selectedCustomDate ->
            let
                selectedPreset =
                    List.filter (\p -> presetToDisplayString p model.languageConfig == selectedCustomDate) model.presets
            in
            case List.head selectedPreset of
                Just preset ->
                    Preset preset

                Nothing ->
                    Unselected


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
            threeMonthCalendarView today zone model visibleRange

        TwoMonths ->
            twoMonthCalendarView today zone model visibleRange

        OneMonth ->
            monthCalendarTableView today zone model visibleRange


yearCalendarView : Posix -> Zone -> Model -> PosixRange -> Html Msg
yearCalendarView today zone model visibleRange =
    let
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
        , table [] [ tbody [ Attrs.class "year" ] <| List.map (\m -> monthCalendarView m today zone model) (getMonthsFromRange 0 11 zone visibleRange getFirstDayOfYear) ]
        ]


threeMonthCalendarView : Posix -> Zone -> Model -> PosixRange -> Html Msg
threeMonthCalendarView today zone model visibleRange =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "three-month-calendar" ]
        [ table [ Attrs.class "month-table" ]
            [ tbody [ Attrs.class "three-month" ] <|
                List.map (\m -> monthCalendarView m today zone model) (getMonthsFromRange 0 2 zone visibleRange getFirstDayOfMonth)
            ]
        ]


twoMonthCalendarView : Posix -> Zone -> Model -> PosixRange -> Html Msg
twoMonthCalendarView today zone model visibleRange =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "two-month-calendar" ]
        [ table [ Attrs.class "month-table" ]
            [ tbody [ Attrs.class "two-month" ] <|
                List.map (\m -> monthCalendarView m today zone model) (getMonthsFromRange 0 1 zone visibleRange getFirstDayOfMonth)
            ]
        ]


monthCalendarTableView : Posix -> Zone -> Model -> PosixRange -> Html Msg
monthCalendarTableView today zone model visibleRange =
    let
        posixMonth =
            getFirstDayOfMonth zone visibleRange.start
    in
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table [ Attrs.class "month-table" ] [ tbody [ Attrs.class "month-body" ] [ monthCalendarView posixMonth today zone model ] ]
        ]


getMonthsFromRange : Int -> Int -> Zone -> PosixRange -> (Zone -> Posix -> Posix) -> List Posix
getMonthsFromRange start end zone visibleRange fn =
    List.map
        (\x ->
            addMonths x utc <| fn zone visibleRange.start
        )
    <|
        List.range start end


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
                [ text <| monthFormatter model.languageConfig utc currentMonth ]
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
            contentIsInCorrectMonth && (Maybe.withDefault False <| Maybe.map (\p -> isSameDay p currentDay) (getPosixFromSelection model.selection today zone))

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


selectionEnd : Selection -> Posix -> Zone -> Maybe Posix
selectionEnd selection today localZone =
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
            Just <| .end <| presetToPosixRange presetType today localZone


selectionStart : Selection -> Posix -> Zone -> Maybe Posix
selectionStart selection today localZone =
    case selection of
        Single posix ->
            Just posix

        Range posixRange ->
            Just posixRange.start

        Unselected ->
            Nothing

        Selecting posixRange ->
            Just <| .start <| normalizeSelectingRange posixRange

        Preset presetType ->
            Just <| .start <| presetToPosixRange presetType today localZone


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


getVisibleRangeFromSelection : Selection -> Posix -> Zone -> Maybe PosixRange
getVisibleRangeFromSelection selection today localZone =
    case Debug.log "test sel" selection of
        Single posix ->
            Just { start = getStartOfDay posix, end = getEndOfDay posix }

        Range posixRange ->
            Just posixRange

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        Preset presetType ->
            let
                a =
                    Debug.log "test" presetType
            in
            Just <| presetToPosixRange presetType today localZone


prettyFormatSelection : Selection -> LanguageConfig -> String
prettyFormatSelection selection languageConfig =
    -- todo handling time zones
    case selection of
        Single posix ->
            singleFormatter languageConfig utc posix

        Range posixRange ->
            fullFormatter languageConfig utc posixRange.start posixRange.end

        Unselected ->
            ""

        Selecting _ ->
            ""

        Preset presetType ->
            presetToDisplayString presetType languageConfig


singleFormatter : LanguageConfig -> Zone -> Posix -> String
singleFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]


monthFormatter : LanguageConfig -> Zone -> Posix -> String
monthFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameFull
        ]


fullFormatter : LanguageConfig -> Zone -> Posix -> Posix -> String
fullFormatter language zone start end =
    singleFormatter language zone start ++ " to " ++ singleFormatter language zone end



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



setCalendarType : CalendarType -> Model -> Model
setCalendarType calendarType model =
    { model | calendarType = calendarType}


--------------------------------------------------------------------------------------------
--            PRESETS
--------------------------------------------------------------------------------------------


presetToDisplayString : PresetType -> LanguageConfig -> String
presetToDisplayString presetType language =
    case presetType of
        Today ->
            language.today

        Yesterday ->
            language.yesterday

        PastWeek ->
            language.pastWeek

        PastMonth ->
            language.pastMonth

        PastYear ->
            language.pastYear

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


downArrow : Html msg
downArrow =
    svg [ Svg.width "20", Svg.height "20", Svg.viewBox "0 0 30 30" ]
        [ g [ Svg.stroke "none", Svg.strokeWidth "1", Svg.fill "none", Svg.fillRule "evenodd", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
            [ g [ Svg.transform "translate(15.000000, 15.000000) scale(-1, 1) rotate(90.000000) translate(-15.000000, -15.000000) translate(12.000000, 9.000000)", Svg.stroke "black", Svg.strokeWidth "2" ]
                [ Svg.polyline [ Svg.points "0 12 6 6 0 0" ]
                    []
                ]
            ]
        ]
